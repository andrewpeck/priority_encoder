import math
import os
import pytest

from cocotb_test.simulator import run

import cocotb
from cocotb.triggers import Timer
from cocotb.clock import Clock
from cocotb.triggers import FallingEdge
from cocotb.triggers import RisingEdge
from cocotb.triggers import Event
import random

# from https://github.com/cocotb/cocotb/blob/master/tests/test_cases/test_discovery/test_discovery.py
# @cocotb.test()
# async def recursive_discover(dut):
#     """Discover absolutely everything in the DUT"""
#     def _discover(obj):
#         for thing in obj:
#             dut._log.info("Found %s (%s)", thing._name, type(thing))
#             _discover(thing)
#     _discover(dut)

#  good example:
#  https://github.com/alexforencich/verilog-ethernet/blob/master/tb/ptp_clock_cdc/test_ptp_clock_cdc.py


def num_pipeline_ffs(depth, increment):
    n = 0
    for i in range(1, depth):
        if (i % increment == 0):
            n = n + 1
    return n


def tree_depth(width, tail=0):
    if (width in [0, 1, 2, 3]):
        return 1+tail
    else:
        return (tree_depth(math.ceil(width/2), tail+1))


def get_latency(dut):
    width = dut.WIDTH.value
    reg_input = dut.REG_INPUT.value
    reg_output = dut.REG_OUTPUT.value
    reg_stages = dut.REG_STAGES.value

    depth = tree_depth(width)
    num_ffs = num_pipeline_ffs(depth, reg_stages)

    latency = reg_input + reg_output + num_ffs
    return latency


@cocotb.test()
async def priority_encoder_random_data(dut):
    """Test for priority encoder with randomized data on all inputs"""

    cocotb.fork(Clock(dut.clock, 20, units="ns").start())  # Create a clock

    width = dut.WIDTH.value
    datbits = dut.DAT_BITS.value
    qltbits = dut.QLT_BITS.value
    qltmask = 2**(qltbits)-1

    for ichn in range(0, width):
        dut.dat_i[ichn] <= 0

    for loop in range(10):
        await RisingEdge(dut.clock)  # Synchronize with the clock

    for loop in range(100):

        # turn on stimulus for 1 clock
        await RisingEdge(dut.clock)  # Synchronize with the clock
        for ichn in range(0, width):
            dut.dat_i[ichn] <= random.randint(0, 2**datbits-1)
        dut.dav_i <= 1

        # turn off after 1 clock
        await RisingEdge(dut.clock)  # Synchronize with the clock

        dut.dav_i <= 0
        for ichn in range(0, width):
            dut.dat_i[ichn] <= 0

        # find the best output
        # (priority encoder emulation)
        best = -1
        qlt = -1
        dat = -1
        for ichn in range(0, width):
            val = int(dut.dat_i[ichn].value)
            print(hex(qltmask & val))
            if ((qltmask & val) > qlt):
                best = ichn
                qlt = val & qltmask
                dat = val

        await RisingEdge(dut.clock)  # Synchronize with the clock
        while (dut.dav_o.value == 0):
            print("dav=" + hex(int(dut.dav_o.value)))
            await RisingEdge(dut.clock)  # Synchronize with the clock

        print("best adr=" + hex(int(best)))
        print("best data=" + hex(int(qlt)))
        print("found adr=" + hex(int(dut.adr_o.value)))
        print("found data=" + hex(int(dut.dat_o.value)))

        assert int(dut.adr_o.value) == best
        assert int(dut.dat_o.value) == dat


@pytest.mark.parametrize("qlt_aspect", [1, 2, 4])
@pytest.mark.parametrize("datbits", [1, 2, 3, 32])
@pytest.mark.parametrize("width", [2, 3, 4, 5, 7, 13, 16, 32, 64, 128])
def test_priority_encoder(width, datbits, qlt_aspect):

    tests_dir = os.path.abspath(os.path.dirname(__file__))
    rtl_dir = os.path.abspath(os.path.join(tests_dir, '..', 'hdl'))
    module = os.path.splitext(os.path.basename(__file__))[0]

    vhdl_sources = [
        os.path.join(rtl_dir, f"priority_encoder.vhd"),
        os.path.join(rtl_dir, f"../tb/priority_encoder_inst.vhd")
    ]

    parameters = {}
    parameters['WIDTH'] = width
    parameters['DAT_BITS'] = datbits
    parameters['QLT_BITS'] = int(datbits/qlt_aspect)

    run(
        vhdl_sources=vhdl_sources,
        module=module,       # name of cocotb test module
        compile_args=["-2008"],
        toplevel="priority_encoder_inst",            # top level HDL
        toplevel_lang="vhdl",
        parameters=parameters,
        gui=0
    )


if __name__ == "__main__":
    test_priority_encoder()

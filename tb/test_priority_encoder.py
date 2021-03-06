import math
import os
import pytest

from cocotb_test.simulator  import run

import cocotb
from cocotb.triggers import Timer
from cocotb.clock import Clock
from cocotb.triggers import FallingEdge
from cocotb.triggers import RisingEdge
from cocotb.triggers import Event
import random

# from https://github.com/cocotb/cocotb/blob/master/tests/test_cases/test_discovery/test_discovery.py
#@cocotb.test()
#async def recursive_discover(dut):
#    """Discover absolutely everything in the DUT"""
#    def _discover(obj):
#        for thing in obj:
#            dut._log.info("Found %s (%s)", thing._name, type(thing))
#            _discover(thing)
#    _discover(dut)

# good example:
# https://github.com/alexforencich/verilog-ethernet/blob/master/tb/ptp_clock_cdc/test_ptp_clock_cdc.py

def num_pipeline_ffs(depth,increment):
    n = 0
    for i in range (1,depth):
        if (i % increment == 0):
            n = n + 1
    return n

def tree_depth(width, tail=0):
    if (width in [0,1,2,3]):
        return 1+tail
    else:
        return (tree_depth(math.ceil(width/2),tail+1))

def get_latency(dut):
    width = dut.g_WIDTH.value
    reg_input = dut.g_REG_INPUT.value
    reg_output = dut.g_REG_OUTPUT.value
    reg_stages = dut.g_REG_STAGES.value

    depth = tree_depth(width)
    num_ffs = num_pipeline_ffs(depth, reg_stages)


    latency  =  reg_input + reg_output + num_ffs
    return latency

@cocotb.test()
async def priority_encoder_random_data(dut):
    """Test for priority encoder with randomized data on all inputs"""

    cocotb.fork(Clock(dut.clock, 20, units="ns").start())  # Create a clock

    width = dut.g_WIDTH.value
    latency = get_latency(dut)

    for ichn in range(0, width):
        dut.dat_i[ichn] <= random.randint (0,2**32-1)

    for loop in range(10):
        await RisingEdge(dut.clock)  # Synchronize with the clock

    for loop in range(100):

        # turn on stimulus for 1 clock
        await RisingEdge(dut.clock)  # Synchronize with the clock
        for ichn in range(0, width):
            dut.dat_i[ichn] <= random.randint (0,2**32-1)
        dut.dav_i <= 1;

        # turn off after 1 clock
        await RisingEdge(dut.clock)  # Synchronize with the clock

        dut.dav_i <= 0;
        for ichn in range(0, width):
            dut.dat_i[ichn] <= 0;

        # find the best output
        # (priority encoder emulation)
        best = -1
        max = -1
        for ichn in range(0, width):
            val = int(dut.dat_i[ichn].value)
            if (val > max):
                best = ichn
                max = val

        await RisingEdge(dut.clock)  # Synchronize with the clock
        while (dut.dav_o.value == 0):
           print ("dav=" + hex(int(dut.dav_o.value)))
           await RisingEdge(dut.clock)  # Synchronize with the clock

        assert int(dut.adr_o.value) == best;
        assert int(dut.dat_o.value) == max;

@pytest.mark.parametrize("width", [2, 3, 4, 5, 7, 13, 16, 32, 64, 128, 1536])
def test_priority_encoder(width):

    tests_dir = os.path.abspath(os.path.dirname(__file__))
    rtl_dir = os.path.abspath(os.path.join(tests_dir, '..', 'hdl'))
    module = os.path.splitext(os.path.basename(__file__))[0]

    vhdl_sources = [
        os.path.join(rtl_dir, f"priority_encoder.vhd"),
        os.path.join(rtl_dir, f"../tb/priority_encoder_inst.vhd")
    ]

    parameters = {}
    parameters['g_WIDTH'] = width

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

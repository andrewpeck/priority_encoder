.PHONY : ghdl test doc

all: ghdl test doc

doc:
	@make -C doc/

test:
	@make -C tb/

ghdl:
	@ghdl -a --std=08 --ieee=synopsys hdl/priority_encoder.vhd
	@ghdl -r --std=08 --ieee=synopsys priority_encoder

clean:
	@rm -f tags
	@rm -f *.o *.cf
	@rm -rf tb/__pycache__
	@rm -rf tb/sim_build
	@rm -rf tb/priority_encoder_inst
	@rm -rf hdl/work

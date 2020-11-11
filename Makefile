.PHONY : priority

priority:
	@ghdl -a --std=08 --ieee=synopsys priority_encoder_pkg.vhd priority_encoder.vhd
	@ghdl -e --std=08 --ieee=synopsys priority_encoder
	@./priority_encoder

clean:
	rm -f *.o *.cf

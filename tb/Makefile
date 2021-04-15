test:
	TOPLEVEL_LANG=vhdl SIM=questa pytest -n 4 -v

all: ghdl build

build:
	 vcom -mixedsvvh -work priority_encoder_inst -2008 /home/andrew/Dropbox/me0sf/road/priority_encoder/hdl/priority_encoder.vhd /home/andrew/Dropbox/me0sf/road/priority_encoder/tb/priority_encoder_inst.vhd

ghdl:
	ghdl -a --std=08 ../hdl/priority_encoder.vhd priority_encoder_inst.vhd
	ghdl -r --std=08 priority_encoder_inst

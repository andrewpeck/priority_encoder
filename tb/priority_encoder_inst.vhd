library ieee;
use ieee.std_logic_1164.all;
use ieee.std_logic_misc.all;
use ieee.numeric_std.all;
use ieee.math_real.all;

library work;
use work.priority_encoder_pkg.all;

entity priority_encoder_inst is
  generic(
    VERBOSE : boolean := true;

    WIDTH : integer := 7;               -- number of inputs

    REG_INPUT  : boolean := false;      -- add ffs to input stage
    REG_OUTPUT : boolean := true;       -- add ffs to output stage
    REG_STAGES : integer := 2;          -- add ffs to every nth pipeline stage

    DAT_BITS   : integer := 32;         -- number of data (non sorting) bits
    QLT_BITS   : integer := 32;         -- number of sorting bits
    ADR_BITS_o : integer := integer(ceil(log2(real(WIDTH))));
    STAGE      : integer := 0           -- set to zero for top level instance

    );
  port(
    clock : in std_logic;

    dav_i : in  std_logic := '0';
    dav_o : out std_logic;

    -- inputs
    dat_i : in bus_array (0 to WIDTH-1)(DAT_BITS -1 downto 0);  -- "extra" non-sorting data bits for each input

    -- outputs
    dat_o : out std_logic_vector (DAT_BITS -1 downto 0)  := (others => '0');
    adr_o : out std_logic_vector (ADR_BITS_o-1 downto 0) := (others => '0')
    );
end priority_encoder_inst;

architecture behavioral of priority_encoder_inst is
begin

  priority_encoder_inst2 : entity work.priority_encoder
    generic map (
      VERBOSE    => VERBOSE,
      WIDTH      => WIDTH,
      REG_INPUT  => REG_INPUT,
      REG_OUTPUT => REG_OUTPUT,
      REG_STAGES => REG_STAGES,
      DAT_BITS   => DAT_BITS,
      QLT_BITS   => QLT_BITS,
      ADR_BITS_o => ADR_BITS_o,
      STAGE      => STAGE)
    port map (
      clock => clock,
      dav_i => dav_i,
      dav_o => dav_o,
      dat_i => dat_i,
      dat_o => dat_o,
      adr_o => adr_o
      );

end behavioral;

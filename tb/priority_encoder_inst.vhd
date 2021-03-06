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

    g_WIDTH : integer := 7;            -- number of inputs

    g_REG_INPUT  : boolean := false;    -- add ffs to input stage
    g_REG_OUTPUT : boolean := true;     -- add ffs to output stage
    g_REG_STAGES : integer := 2;        -- add ffs to every nth pipeline stage

    g_DAT_SIZE   : integer := 32;                                  -- number of data (non sorting) bits
    g_QLT_SIZE   : integer := 32;                                  -- number of sorting bits
    g_ADR_SIZE_i : integer := 0;                                   -- set to zero for top level instance
    g_ADR_SIZE_o : integer := integer(ceil(log2(real(g_WIDTH))));
    g_STAGE      : integer := 0                                    -- set to zero for top level instance

    );
  port(
    clock : in std_logic;

    dav_i : in std_logic := '0';
    dav_o : out std_logic;

    -- inputs
    dat_i : in bus_array (0 to g_WIDTH-1)(g_DAT_SIZE -1 downto 0);                                  -- "extra" non-sorting data bits for each input

    -- outputs
    dat_o : out std_logic_vector (g_DAT_SIZE -1 downto 0) := (others => '0');
    adr_o : out std_logic_vector (g_ADR_SIZE_o-1 downto 0):= (others => '0')
    );
end priority_encoder_inst;

architecture behavioral of priority_encoder_inst is
begin

  priority_encoder_inst : entity work.priority_encoder
    generic map (
      VERBOSE      => VERBOSE,
      g_WIDTH      => g_WIDTH,
      g_REG_INPUT  => g_REG_INPUT,
      g_REG_OUTPUT => g_REG_OUTPUT,
      g_REG_STAGES => g_REG_STAGES,
      g_DAT_SIZE   => g_DAT_SIZE,
      g_QLT_SIZE   => g_QLT_SIZE,
      g_ADR_SIZE_i => g_ADR_SIZE_i,
      g_ADR_SIZE_o => g_ADR_SIZE_o,
      g_STAGE      => g_STAGE)
    port map (
      clock => clock,
      dav_i => dav_i,
      dav_o => dav_o,
      dat_i => dat_i,
      dat_o => dat_o,
      adr_o => adr_o
      );

end behavioral;

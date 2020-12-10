-------------------------------------------------------------------------------
-- Title      : Priority Encoder
-------------------------------------------------------------------------------
-- File       : priority_encoder.vhd
-- Author     : Andrew Peck  <andrew.peck@cern.ch>
-- Last update: 2020-12-03
-- Standard   : VHDL'2008
-------------------------------------------------------------------------------
-- Description:
--   Weighted, pipelined, priority encoder.
--
--   Given an array of inputs, it will choose the first (least significant)
--   output.
--
--   Inputs are weighted by a quality and sorted based on this.
--   The quality field should be the first (least significant)
--   N bits of the data field.
--
--   To do unweighted sorting (e.g. based on a valid bit),
--   just set g_QLT_SIZE to 1 and make the
--   valid bit the LSB of the data word
--
--   Both the data itself as well as the address of the data are output
--   from the module
-------------------------------------------------------------------------------

--TODO: need to simulate this

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use ieee.math_real.all;

library work;
use work.priority_encoder_pkg.all;

entity priority_encoder is
  generic(
    VERBOSE : boolean := false;

    g_WIDTH : integer := 11;            -- number of inputs

    g_REG_INPUT  : boolean := false;    -- add ffs to input stage
    g_REG_OUTPUT : boolean := true;     -- add ffs to output stage
    g_REG_STAGES : integer := 2;        -- add ffs to every nth pipeline stage

    g_DAT_SIZE   : integer := 1;                                   -- number of data (non sorting) bits
    g_QLT_SIZE   : integer := 1;                                   -- number of sorting bits
    g_ADR_SIZE_i : integer := 0;                                   -- set to zero for top level instance
    g_ADR_SIZE_o : integer := integer(ceil(log2(real(g_WIDTH))));
    g_STAGE      : integer := 0                                    -- set to zero for top level instance

    );
  port(
    clock : in std_logic;

    -- inputs
    dat_i : in bus_array (0 to g_WIDTH-1)(g_DAT_SIZE -1 downto 0);                                  -- "extra" non-sorting data bits for each input
    adr_i : in bus_array (0 to g_WIDTH-1)(g_ADR_SIZE_i-1 downto 0) := (others => (others => '0'));  -- address bits, set to zero for top level

    -- outputs
    dat_o : out std_logic_vector (g_DAT_SIZE -1 downto 0);
    adr_o : out std_logic_vector (g_ADR_SIZE_o-1 downto 0)
    );
end priority_encoder;

architecture behavioral of priority_encoder is

  function adrcat (adr : std_logic_vector;
                   base : std_logic_vector;
                   N : integer)
    return std_logic_vector is
  begin
    if (N = 0) then
      return adr;
    else
      return adr & base;
    end if;
  end function;

  function adrcat (adr : std_logic;
                   base : std_logic_vector;
                   N : integer)
    return std_logic_vector is
  begin
    if (N = 0) then
      return "" & adr;
    else
      return adr & base;
    end if;
  end function;


  function quality (slv : std_logic_vector) return std_logic_vector is
    variable result : std_logic_vector(g_QLT_SIZE-1 downto 0);
  begin
    return slv(g_QLT_SIZE-1 downto 0);
  end;

  procedure best_1of2 (
    signal best_adr, best_dat : out std_logic_vector;
    adr0, adr1, dat0, dat1    : in  std_logic_vector
    ) is
  begin
    if (quality(dat1) > quality(dat0)) then

      best_adr <= adrcat ('1', adr1, g_ADR_SIZE_i);
      best_dat <= dat1;
    else
      best_adr <= adrcat ('0', adr0, g_ADR_SIZE_i);
      best_dat <= dat0;
    end if;
  end best_1of2;

  function next_width (width : integer)
    return integer is
  begin
    -- for size=4 we reduce to 2 and add 1 bit
    -- for size=3 we reduce to 1 and add 2 bits
    -- for size=2 we reduce to 1 and add 1 bit
    if (width = 1) then
      return 1;
    elsif (width = 3) then
      return 1;
    elsif (width mod 2 = 0) then
      return width / 2;
    else
      return (width+1)/ 2;
    end if;
  end function;

  function extra_adrb (width : integer)
    return integer is
  begin
    -- for size=4 we reduce to 2 and add 1 bit
    -- for size=3 we reduce to 1 and add 2 bits
    -- for size=2 we reduce to 1 and add 1 bit
    if (width = 1) then
      return 0;
    elsif (width = 3) then
      return 2;
    else
      return 1;
    end if;
  end function;

begin

  assert not VERBOSE report "Generating priority encoder stage " & integer'image(g_STAGE) severity note;
  assert not VERBOSE report "  WIDTH= " & integer'image(g_WIDTH) severity note;
  assert not VERBOSE report "  ADRBI= " & integer'image(g_ADR_SIZE_i) severity note;
  assert not VERBOSE report "  ADRBO= " & integer'image(adr_o'length) severity note;

  -- do a 2:1 reduction of all of the inputs to form a 1/2 width comparison array
  -- feed this recursively into another encoder which will have 1 additional addrb
  comp_gen : if (g_WIDTH > 3) generate
    constant comp_out_width : integer := next_width(g_WIDTH);
    signal dat              : bus_array (0 to comp_out_width-1)(g_DAT_SIZE-1 downto 0);
    signal adr              : bus_array (0 to comp_out_width-1)(g_ADR_SIZE_i downto 0);  -- add 1 bit
  begin

    assert not VERBOSE report " > Generating comparators for #inputs=" & integer'image(g_WIDTH) severity note;

    nzgen : if (g_ADR_SIZE_i > 0) generate
    assert not VERBOSE report " > adr_next (" & integer'image(g_ADR_SIZE_i) &
      " downto 0) <= 'x' & adr_i (" & integer'image(g_ADR_SIZE_i -1 ) & " downto 0)" severity note;
    end generate;

    zgen : if (g_ADR_SIZE_i = 0) generate
    assert not VERBOSE report " > adr_next (" & integer'image(g_ADR_SIZE_i) &
      " downto 0) <= 'x'" severity note;
    end generate;

    comp_loop : for icomp in 0 to comp_out_width-1 generate
    begin


      -- even cases are simple
      gen_even : if (icomp < comp_out_width -1 or (g_WIDTH mod 2 = 0)) generate
        assert not VERBOSE report "   > icomp: #" & integer'image(icomp+1) &
          " of " & integer'image(comp_out_width) &
          " compare: " & integer'image(icomp*2+1) &
          " to " & integer'image(icomp*2) severity note;
        process (clock, adr_i, dat_i) is
        begin
          if (rising_edge(clock) or not (
            (g_STAGE = 0 and g_REG_INPUT) or
            (g_STAGE /= 0 and (g_STAGE mod g_REG_STAGES = 0)))) then
            best_1of2 (adr(icomp), dat(icomp),
                       adr_i(icomp*2), adr_i(icomp*2+1),
                       dat_i(icomp*2), dat_i(icomp*2+1));
          end if;
        end process;
      end generate;

      -- if we have an odd number of inputs, just choose highest # real entry (no comparator)
      gen_odd : if (g_WIDTH mod 2 /= 0 and icomp = comp_out_width-1) generate
        process (clock, adr_i, dat_i) is
        begin
          if (rising_edge(clock) or not (
            (g_STAGE = 0 and g_REG_INPUT) or
            (g_STAGE /= 0 and (g_STAGE mod g_REG_STAGES = 0)))) then
            assert not VERBOSE report "  > odd nocompare on : " & integer'image(icomp*2) severity note;

            dat(icomp) <= dat_i(icomp*2);
            adr(icomp) <= adrcat ('0', adr_i (icomp*2), g_ADR_SIZE_i);
          end if;
        end process;
      end generate;

    end generate;

    priority_encoder_inst : entity work.priority_encoder
      generic map (
        g_STAGE      => g_STAGE + 1,
        g_REG_STAGES => g_REG_STAGES,
        g_WIDTH      => comp_out_width,
        g_DAT_SIZE   => g_DAT_SIZE,
        g_QLT_SIZE   => g_QLT_SIZE,
        g_ADR_SIZE_i => g_ADR_SIZE_i+1,
        g_ADR_SIZE_o => g_ADR_SIZE_o
        )
      port map (
        clock => clock,
        dat_i => dat,
        adr_i => adr,
        dat_o => dat_o,
        adr_o => adr_o
        );

  end generate;

  --------------------------------------------------------------------------------
  -- handle the final special cases
  --------------------------------------------------------------------------------

  -- for a single final case, just output it
  g_WIDTH1_gen : if (g_WIDTH = 1) generate
    adr_o <= adr_i(0);
    dat_o <= dat_i(0);
  end generate;

  -- for a double final case, choose 1 of 2
  g_WIDTH2_gen : if (g_WIDTH = 2) generate
    assert not VERBOSE report "   > 2:1 mux" severity note;
    process (clock, adr_i, dat_i) is
    begin
      if (rising_edge(clock) or (not g_REG_OUTPUT)) then
        best_1of2 (adr_o, dat_o,
                   adr_i(0), adr_i(1),
                   dat_i(0), dat_i(1));
      end if;
    end process;
  end generate;

  -- for a triple final case, choose 1 of 3
  g_WIDTH3_gen : if (g_WIDTH = 3) generate
    assert not VERBOSE report "   > 3:1 mux" severity note;

    nzgen : if (g_ADR_SIZE_i > 0) generate
    assert not VERBOSE report
      " > adr_o (" & integer'image(g_ADR_SIZE_o-1) &
      " downto 0) <= 'xx' & adr_i ("
      & integer'image(g_ADR_SIZE_i -1 ) & "downto 0)" severity note;
    end generate;

    zgen : if (g_ADR_SIZE_i = 0) generate
    assert not VERBOSE report
      " > adr_o (" & integer'image(g_ADR_SIZE_o-1) &
      " downto 0) <= 'xx'" severity note;
    end generate;

    process (clock, adr_i, dat_i) is
    begin
      if (rising_edge(clock) or (not g_REG_OUTPUT)) then
        -- choose 2
        if (quality(dat_i(2)) > quality(dat_i (1)) and quality(dat_i(2)) > quality(dat_i (0))) then
          adr_o <= adrcat ("10", adr_i (2), g_ADR_SIZE_i);
          dat_o <= dat_i (2);
        -- choose 1
        elsif (quality(dat_i(1)) > quality(dat_i (0))) then
          adr_o <= adrcat ("01", adr_i (1), g_ADR_SIZE_i);
          dat_o <= dat_i (1);
        -- choose 0
        else
          adr_o <= adrcat ("00", adr_i (0), g_ADR_SIZE_i);
          dat_o <= dat_i (0);
        end if;
      end if;
    end process;

  end generate;

end behavioral;

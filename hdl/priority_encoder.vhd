-------------------------------------------------------------------------------
-- Title      : Priority Encoder
-------------------------------------------------------------------------------
-- File       : priority_encoder.vhd
-- Author     : Andrew Peck  <andrew.peck@cern.ch>
-- Last update: 2021-03-05
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
--   just set QLT_BITS to 1 and make the
--   valid bit the LSB of the data word
--
--   Both the data itself as well as the address of the data are output
--   from the module
-------------------------------------------------------------------------------

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

package priority_encoder_pkg is
  type bus_array is array(natural range <>) of std_logic_vector;
end package;

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use ieee.math_real.all;

library work;
use work.priority_encoder_pkg.all;

entity priority_encoder is
  generic(
    VERBOSE : boolean := false;

    WIDTH : integer := 5;               -- number of inputs

    REG_INPUT  : boolean := false;      -- add ffs to input stage
    REG_OUTPUT : boolean := true;       -- add ffs to output stage
    REG_STAGES : integer := 2;          -- add ffs to every nth pipeline stage

    DAT_BITS   : integer := 32;         -- number of data (non sorting) bits
    QLT_BITS   : integer := 1;          -- number of sorting bits
    ADR_BITS_i : integer := 0;          -- set to zero for top level instance
    ADR_BITS_o : integer := integer(ceil(log2(real(WIDTH))));
    STAGE      : integer := 0           -- set to zero for top level instance

    );
  port(
    clock : in std_logic;

    -- inputs
    dat_i : in bus_array (0 to WIDTH-1)(DAT_BITS -1 downto 0)  := (others => (others => '0'));  -- "extra" non-sorting data bits for each input
    adr_i : in bus_array (0 to WIDTH-1)(ADR_BITS_i-1 downto 0) := (others => (others => '0'));  -- address bits, set to zero for top level
    dav_i : in std_logic                                         := '0';

    -- outputs
    dat_o : out std_logic_vector (DAT_BITS -1 downto 0)  := (others => '0');
    adr_o : out std_logic_vector (ADR_BITS_o-1 downto 0) := (others => '0');
    dav_o : out std_logic                                  := '0'
    );
end priority_encoder;

architecture behavioral of priority_encoder is

  -- FIXME: DOCUMENT ME
  function adrcat (adr : std_logic_vector; base : std_logic_vector; N : integer)
    return std_logic_vector is
  begin
    if (N <= 0) then
      return adr;
    else
      return adr & base;
    end if;
  end function;

  -- FIXME: DOCUMENT ME
  function adrcat (adr : std_logic; base : std_logic_vector; N : integer)
    return std_logic_vector is
  begin
    if (N = 0) then
      return "" & adr;
    else
      return adr & base;
    end if;
  end function;

  -- FIXME: DOCUMENT ME
  function quality (slv : std_logic_vector) return std_logic_vector is
    variable result : std_logic_vector(QLT_BITS-1 downto 0);
  begin
    return slv(QLT_BITS-1 downto 0);
  end;

  -- FIXME: DOCUMENT ME
  procedure best_1of2 (
    signal best_adr, best_dat : out std_logic_vector;
    adr0, adr1, dat0, dat1    : in  std_logic_vector
    ) is
  begin
    if (quality(dat1) > quality(dat0)) then
      best_adr <= adrcat ('1', adr1, ADR_BITS_i);
      best_dat <= dat1;
    else
      best_adr <= adrcat ('0', adr0, ADR_BITS_i);
      best_dat <= dat0;
    end if;
  end best_1of2;

  -- FIXME: DOCUMENT ME
  function next_width (current_width : integer)
    return integer is
  begin
    -- for size=4 we reduce to 2 and add 1 bit
    -- for size=3 we reduce to 1 and add 2 bits
    -- for size=2 we reduce to 1 and add 1 bit
    if (current_width = 1) then
      return 1;
    elsif (current_width = 3) then
      return 1;
    elsif (current_width mod 2 = 0) then
      return current_width / 2;
    else
      return (current_width+1)/ 2;
    end if;
  end function;

  -- FIXME: DOCUMENT ME
  function extra_adrb (current_width : integer)
    return integer is
  begin
    -- for size=4 we reduce to 2 and add 1 bit
    -- for size=3 we reduce to 1 and add 2 bits
    -- for size=2 we reduce to 1 and add 1 bit
    if (current_width = 1) then
      return 0;
    elsif (current_width = 3) then
      return 2;
    else
      return 1;
    end if;
  end function;

  -- FIXME: DOCUMENT ME
  function stage_is_registered (stg : integer; reg_stgs : integer; reg_inp : boolean)
    return boolean is
  begin
    return ((stg = 0 and reg_inp) or (stg /= 0 and (stg mod reg_stgs = 0)));
  end function;

  signal dav : std_logic := '0';

begin

  assert not VERBOSE report "Generating priority encoder stage " & integer'image(STAGE) severity note;
  assert not VERBOSE report "  WIDTH= " & integer'image(WIDTH) severity note;
  assert not VERBOSE report "  ADRBI= " & integer'image(ADR_BITS_i) severity note;
  assert not VERBOSE report "  ADRBO= " & integer'image(adr_o'length) severity note;

  -- do a 2:1 reduction of all of the inputs to form a 1/2 width comparison array
  -- feed this recursively into another encoder which will have 1 additional addrb
  comp_gen : if (WIDTH > 3) generate
    constant comp_out_width : integer                                                  := next_width(WIDTH);
    signal dat              : bus_array (0 to comp_out_width-1)(DAT_BITS-1 downto 0) := (others => (others => '0'));
    signal adr              : bus_array (0 to comp_out_width-1)(ADR_BITS_i downto 0) := (others => (others => '0'));  -- add 1 bit
  begin

    assert not VERBOSE report " > Generating comparators for #inputs=" & integer'image(WIDTH) severity note;

    process (clock, dav_i) is
    begin
      if (rising_edge(clock) or not (stage_is_registered(STAGE, REG_STAGES, REG_INPUT))) then
        dav <= dav_i;
      end if;
    end process;

    comp_loop : for icomp in 0 to comp_out_width-1 generate
    begin

      -- even cases are simple
      gen_even : if (icomp < comp_out_width -1 or (WIDTH mod 2 = 0)) generate

        assert not VERBOSE report "   > icomp: #" & integer'image(icomp+1) &
          " of " & integer'image(comp_out_width) &
          " compare: " & integer'image(icomp*2+1) &
          " to " & integer'image(icomp*2) severity note;

        process (clock, adr_i, dat_i) is
        begin
          if (rising_edge(clock) or not (stage_is_registered(STAGE, REG_STAGES, REG_INPUT))) then
            best_1of2 (adr(icomp), dat(icomp),
                       adr_i(icomp*2), adr_i(icomp*2+1),
                       dat_i(icomp*2), dat_i(icomp*2+1));
          end if;
        end process;

      end generate;

      -- if we have an odd number of inputs, just choose highest # real entry (no comparator)
      gen_odd : if (WIDTH mod 2 /= 0 and icomp = comp_out_width-1) generate

        assert not VERBOSE report "  > odd nocompare on : " & integer'image(icomp*2) severity note;

        process (clock, adr_i, dat_i) is
        begin
          if (rising_edge(clock) or not (stage_is_registered(STAGE, REG_STAGES, REG_INPUT))) then
            --dat(icomp) <= dat_i(icomp*2);
            --adr(icomp) <= adrcat ('0', adr_i (icomp*2), ADR_BITS_i);
            best_1of2 (adr(icomp), dat(icomp),
                       adr_i(icomp*2), adr_i(icomp*2),
                       dat_i(icomp*2), dat_i(icomp*2));
          end if;
        end process;

      end generate;
    end generate;

    -- recursively generate this module to continue down the chain...

    priority_encoder_inst : entity work.priority_encoder
      generic map (
        STAGE      => STAGE + 1,
        REG_STAGES => REG_STAGES,
        WIDTH      => comp_out_width,
        DAT_BITS   => DAT_BITS,
        QLT_BITS   => QLT_BITS,
        ADR_BITS_i => ADR_BITS_i+1,
        ADR_BITS_o => ADR_BITS_o
        )
      port map (
        clock => clock,
        dav_i => dav,
        dat_i => dat,
        adr_i => adr,
        dav_o => dav_o,
        dat_o => dat_o,
        adr_o => adr_o
        );

  end generate;

  --------------------------------------------------------------------------------
  -- handle the final special cases
  --------------------------------------------------------------------------------

  -- for a single final case, just output it
  WIDTH1_gen : if (WIDTH = 1) generate
  begin
    process (clock, dav_i, adr_i, dat_i) is
    begin
      if (rising_edge(clock) or (not REG_OUTPUT)) then
        dav_o <= dav_i;
        adr_o <= adr_i(0);
        dat_o <= dat_i(0);
      end if;
    end process;
  end generate;

  -- for a double final case, choose 1 of 2
  WIDTH2_gen : if (WIDTH = 2) generate
    assert not VERBOSE report "   > 2:1 mux" severity note;
    process (clock, dav_i, adr_i, dat_i) is
    begin
      if (rising_edge(clock) or (not REG_OUTPUT)) then
        dav_o <= dav_i;
        best_1of2 (adr_o, dat_o,
                   adr_i(0), adr_i(1),
                   dat_i(0), dat_i(1));
      end if;
    end process;
  end generate;

  -- for a triple final case, choose 1 of 3
  WIDTH3_gen : if (WIDTH = 3) generate
    assert not VERBOSE report "   > 3:1 mux" severity note;

    process (clock, dav_i, adr_i, dat_i) is
    begin
      if (rising_edge(clock) or (not REG_OUTPUT)) then

        dav_o <= dav_i;

        -- choose 2
        if (quality(dat_i(2)) > quality(dat_i (1)) and quality(dat_i(2)) > quality(dat_i (0))) then
          adr_o <= adrcat ("10", adr_i (2), ADR_BITS_i);
          dat_o <= dat_i (2);
        -- choose 1
        elsif (quality(dat_i(1)) > quality(dat_i (0))) then
          adr_o <= adrcat ("01", adr_i (1), ADR_BITS_i);
          dat_o <= dat_i (1);
        -- choose 0
        else
          adr_o <= adrcat ("00", adr_i (0), ADR_BITS_i);
          dat_o <= dat_i (0);
        end if;
      end if;
    end process;

  end generate;

end behavioral;

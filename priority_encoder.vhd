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

    g_REG_INPUT  : boolean := false;    -- add ffs to input stage
    g_REG_OUTPUT : boolean := true;     -- add ffs to output stage
    g_REG_STAGES : integer := 2;        -- add ffs to every nth pipeline stage

    g_DAT_SIZE   : integer := 1;        -- number of data (non sorting) bits
    g_QLT_SIZE   : integer := 1;        -- number of sorting bits
    g_ADR_SIZE_i : integer := 0;        -- set to zero for top level instance
    g_STAGE      : integer := 0;        -- set to zero for top level instance

    g_WIDTH : integer := 33             -- number of inputs
    );
  port(
    clock : in std_logic;

    -- inputs
    dat_i : in bus_array (0 to g_WIDTH-1)(g_DAT_SIZE -1 downto 0);
    qlt_i : in bus_array (0 to g_WIDTH-1)(g_QLT_SIZE -1 downto 0);
    adr_i : in bus_array (0 to g_WIDTH-1)(g_ADR_SIZE_i-1 downto 0);

    -- outputs
    dat_o : out std_logic_vector (g_DAT_SIZE -1 downto 0);
    qlt_o : out std_logic_vector (g_QLT_SIZE -1 downto 0);
    adr_o : out std_logic_vector (integer(ceil(log2(real(g_WIDTH))))-1 downto 0)
    );
end priority_encoder;

architecture behavioral of priority_encoder is

  procedure best_1of2 (
    signal best_qlt, best_adr, best_dat : out std_logic_vector;
    qlt0, qlt1, adr0, adr1, dat0, dat1  : in  std_logic_vector
    ) is
  begin
    if (qlt1 > qlt0) then
      best_adr <= ('1' & adr1);
      best_qlt <= qlt1;
      best_dat <= dat1;
    else
      best_adr <= ('0' & adr0);
      best_qlt <= qlt0;
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

begin

  assert not VERBOSE report "Generating priority encoder" severity note;
  assert not VERBOSE report "  ADRBI= " & integer'image(g_ADR_SIZE_i+1) severity note;
  assert not VERBOSE report "  ADRBO= " & integer'image(adr_o'length) severity note;

  -- do a 2:1 reduction of all of the inputs to form a 1/2 width comparison array
  -- feed this recursively into another encoder which will have 1 additional addrb
  comp_gen : if (g_WIDTH > 3) generate
    constant comp_out_width : integer := next_width(g_WIDTH);
    signal dat              : bus_array (0 to comp_out_width-1)(g_DAT_SIZE-1 downto 0);
    signal qlt              : bus_array (0 to comp_out_width-1)(g_QLT_SIZE-1 downto 0);
    signal adr              : bus_array (0 to comp_out_width-1)(g_ADR_SIZE_i downto 0);  -- add 1 bit
  begin

    assert not VERBOSE report " > Generating comparators for #inputs=" & integer'image(g_WIDTH) severity note;

    comp_loop : for icomp in 0 to comp_out_width-1 generate
    begin

      -- even cases are simple
      gen_even : if (icomp < comp_out_width -1 or (g_WIDTH mod 2 = 0)) generate
        assert not VERBOSE report "   > icomp: #" & integer'image(icomp+1) & " of " & integer'image(comp_out_width) & " compare: " & integer'image(icomp*2+1) & " to " & integer'image(icomp*2) severity note;
        process (clock) is
        begin
          if (rising_edge(clock) or not (
            (g_STAGE = 0 and g_REG_INPUT) or
            (g_STAGE /= 0 and (g_STAGE mod g_REG_STAGES = 0)))) then
            best_1of2 (qlt(icomp), adr(icomp), dat(icomp),
                       qlt_i(icomp*2), qlt_i(icomp*2+1),
                       adr_i(icomp*2), adr_i(icomp*2+1),
                       dat_i(icomp*2), dat_i(icomp*2+1));
          end if;
        end process;
      end generate;

      -- if we have an odd number of inputs, just choose highest # real entry (no comparator)
      gen_odd : if (g_WIDTH mod 2 /= 0 and icomp = comp_out_width-1) generate
        process (clock) is
        begin
          if (rising_edge(clock) or not (
            (g_STAGE = 0 and g_REG_INPUT) or
            (g_STAGE /= 0 and (g_STAGE mod g_REG_STAGES = 0)))) then
            assert not VERBOSE report "  > odd nocompare on : " & integer'image(icomp*2) severity note;
            dat(icomp) <= dat_i(icomp*2);
            qlt(icomp) <= qlt_i(icomp*2);
            adr(icomp) <= '0' & adr_i(icomp*2);
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
        g_ADR_SIZE_i => g_ADR_SIZE_i+1  -- add 1 to next stage input
        )
      port map (
        clock => clock,
        dat_i => dat,
        qlt_i => qlt,
        adr_i => adr,
        dat_o => dat_o,
        qlt_o => qlt_o,
        adr_o => adr_o
        );

  end generate;

  --------------------------------------------------------------------------------
  -- handle the final special cases
  --------------------------------------------------------------------------------

  -- for a single final case, just output it
  g_WIDTH1_gen : if (g_WIDTH = 1) generate
    qlt_o <= qlt_i(0);
    adr_o <= adr_i(0);
    dat_o <= dat_i(0);
  end generate;

  -- for a double final case, choose 1 of 2
  g_WIDTH2_gen : if (g_WIDTH = 2) generate
    assert not VERBOSE report "   > 2:1 mux" severity note;
    process (clock) is
    begin
      if (rising_edge(clock) or (not g_REG_OUTPUT)) then
        best_1of2 (qlt_o, adr_o, dat_o,
                   qlt_i(0), qlt_i(1),
                   adr_i(0), adr_i(1),
                   dat_i(0), dat_i(1));
      end if;
    end process;
  end generate;

  -- for a triple final case, choose 1 of 3
  g_WIDTH3_gen : if (g_WIDTH = 3) generate
    assert not VERBOSE report "   > 3:1 mux" severity note;
    process (clock) is
    begin
      if (rising_edge(clock) or (not g_REG_OUTPUT)) then
        -- choose 2
        if (qlt_i(2) > qlt_i (1) and qlt_i(2) > qlt_i (0)) then
          qlt_o <= qlt_i (2);
          adr_o <= "10" & adr_i (2);
          dat_o <= dat_i (2);
        -- choose 1
        elsif (qlt_i(1) > qlt_i (0)) then
          qlt_o <= qlt_i (1);
          adr_o <= "01" & adr_i (1);
          dat_o <= dat_i (1);
        else
          qlt_o <= qlt_i (0);
          adr_o <= "00" & adr_i (0);
          dat_o <= dat_i (0);
        end if;
      end if;
    end process;

  end generate;

end behavioral;

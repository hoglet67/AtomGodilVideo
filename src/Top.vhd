----------------------------------------------------------------------------------
-- Company: 
-- Engineer: 
-- 
-- Create Date:    14:42:09 02/09/2013 
-- Design Name: 
-- Module Name:    Top - Behavioral 
-- Project Name: 
-- Target Devices: 
-- Tool versions: 
-- Description: 
--
-- Dependencies: 
--
-- Revision: 
-- Revision 0.01 - File Created
-- Additional Comments: 
--
----------------------------------------------------------------------------------
library IEEE;
use IEEE.STD_LOGIC_1164.all;

-- Uncomment the following library declaration if using
-- arithmetic functions with Signed or Unsigned values
use IEEE.NUMERIC_STD.all;
use IEEE.STD_LOGIC_UNSIGNED.all;

-- Uncomment the following library declaration if instantiating
-- any Xilinx primitives in this code.
--library UNISIM;
--use UNISIM.VComponents.all;

entity Top is
    port (

        -- Standard 6847 signals
        --
        -- expept DA which is now input only
        -- except nRP which re-purposed as a nWR

        CLK    : in    std_logic;
        DD     : inout std_logic_vector (7 downto 0);
        DA     : in    std_logic_vector (12 downto 0);
        CHB    : out   std_logic;
        OA     : out   std_logic;
        OB     : out   std_logic;
        nMS    : in    std_logic;
        CSS    : in    std_logic;
        nHS    : out   std_logic;
        nFS    : out   std_logic;
        nWR    : in    std_logic;       -- Was nRP
        AG     : in    std_logic;
        AS     : in    std_logic;
        INV    : in    std_logic;
        INTEXT : in    std_logic;
        GM     : in    std_logic_vector (2 downto 0);
        Y      : out   std_logic;

        -- 5 bit VGA Output

        R     : out std_logic_vector (1 downto 0);
        G     : out std_logic_vector (0 downto 0);
        B     : out std_logic_vector (0 downto 0);
        HSYNC : out std_logic;
        VSYNC : out std_logic;
        
        -- 1 bit AUDIO Output
        AUDIO : out std_logic;
        
        -- Other GODIL specific pins

        clock49 : in std_logic;
        nRST : in std_logic;

        -- Test Pins

        Test1 : out std_logic;
        Test2 : out std_logic;
        Test3 : out std_logic

        );
end Top;

architecture BEHAVIORAL of Top is

    -- Set this to 0 if you want dark green/dark orange background on text
    -- Set this to 1 if you want black background on text (authentic Atom)
    constant BLACK_BACKGND : std_logic := '1';

    -- Clock12 is a half speed VGA clock
    signal clock12 : std_logic;
    
    -- Other clocks are for SID
    -- 1MHZ, 32MHz and 
    signal div32  : std_logic_vector (4 downto 0);
    signal clock1 : std_logic;
    signal clock15 : std_logic;
    signal clock32 : std_logic;
    
    -- Reset signal to 6847, not currently used
    signal reset   : std_logic;

    -- pipelined versions of the address/data/write signals
    signal nWR1 : std_logic;
    signal nWR2 : std_logic;
    signal nMS1 : std_logic;
    signal nMS2 : std_logic;
    signal DA1  : std_logic_vector (12 downto 0);
    signal DA2  : std_logic_vector (12 downto 0);
    signal DD1  : std_logic_vector (7 downto 0);
    signal DD2  : std_logic_vector (7 downto 0);

    -- VGA colour signals out of mc6847, only top 2 bits are used
    signal vga_red   : std_logic_vector (7 downto 0);
    signal vga_green : std_logic_vector (7 downto 0);
    signal vga_blue  : std_logic_vector (7 downto 0);
    signal vga_vsync : std_logic;
    signal vga_hsync : std_logic;
    
    -- 8Kx8 Dual port video RAM signals
    -- Port A connects to Atom and is read/write
    -- Port B connects to MC6847 and is read only
    signal wr     : std_logic;
    signal addra : std_logic_vector (12 downto 0);
    signal dina  : std_logic_vector (7 downto 0);
    signal douta : std_logic_vector (7 downto 0);
    signal addrb : std_logic_vector (12 downto 0);
    signal doutb : std_logic_vector (7 downto 0);

    -- Dout back to the Atom, that is either VRAM or SID
    signal dout  : std_logic_vector (7 downto 0);

    -- Masked (by nRST) version of the mode control signals
    signal mask  : std_logic;    
    signal gm_masked  : std_logic_vector (2 downto 0);
    signal ag_masked  : std_logic;
    signal css_masked : std_logic;
    

    -- SID sigmals
    signal sid_cs : std_logic;
    signal sid_we : std_logic;
    signal sid_addr  : std_logic_vector (4 downto 0);
    signal sid_do  : std_logic_vector (7 downto 0);
    signal sid_di  : std_logic_vector (7 downto 0);
    signal sid_audio : std_logic;


    component DCM0
        port(
            CLKIN_IN  : in  std_logic;
            CLK0_OUT  : out std_logic;
            CLK0_OUT1 : out std_logic;
            CLK2X_OUT : out std_logic
            );
    end component;

    component DCMSID0
        port(
            CLKIN_IN  : in  std_logic;
            CLK0_OUT  : out std_logic;
            CLK0_OUT1 : out std_logic;
            CLK2X_OUT : out std_logic
            );
    end component;

    component DCMSID1
        port(
            CLKIN_IN  : in  std_logic;
            CLK0_OUT  : out std_logic;
            CLK0_OUT1 : out std_logic;
            CLK2X_OUT : out std_logic
            );
    end component;

    component mc6847
        port(
            clk            : in  std_logic;
            clk_ena        : in  std_logic;
            reset          : in  std_logic;
            dd             : in  std_logic_vector(7 downto 0);
            an_g           : in  std_logic;
            an_s           : in  std_logic;
            intn_ext       : in  std_logic;
            gm             : in  std_logic_vector(2 downto 0);
            css            : in  std_logic;
            inv            : in  std_logic;
            artifact_en    : in  std_logic;
            artifact_set   : in  std_logic;
            artifact_phase : in  std_logic;
            da0            : out std_logic;
            videoaddr      : out std_logic_vector(12 downto 0);
            hs_n           : out std_logic;
            fs_n           : out std_logic;
            red            : out std_logic_vector(7 downto 0);
            green          : out std_logic_vector(7 downto 0);
            blue           : out std_logic_vector(7 downto 0);
            hsync          : out std_logic;
            vsync          : out std_logic;
            hblank         : out std_logic;
            vblank         : out std_logic;
            cvbs           : out std_logic_vector(7 downto 0);
            black_backgnd  : in  std_logic
            );
    end component;

    component VideoRam
        port (
            clka  : in  std_logic;
            wea   : in  std_logic;
            addra : in  std_logic_vector(12 downto 0);
            dina  : in  std_logic_vector(7 downto 0);
            douta : out std_logic_vector(7 downto 0);
            clkb  : in  std_logic;
            web   : in  std_logic;
            addrb : in  std_logic_vector(12 downto 0);
            dinb  : in  std_logic_vector(7 downto 0);
            doutb : out std_logic_vector(7 downto 0)
            );
    end component;
    
    component sid6581
        port(
            clk_1MHz : in std_logic;
            clk32 : in std_logic;
            clk_DAC : in std_logic;
            reset : in std_logic;
            cs : in std_logic;
            we : in std_logic;
            addr : in std_logic_vector(4 downto 0);
            di : in std_logic_vector(7 downto 0);    
            pot_x : in std_logic;
            pot_y : in std_logic;      
            do : out std_logic_vector(7 downto 0);
            audio_out : out std_logic;
            audio_data : out std_logic_vector(17 downto 0)
            );
    end component;

begin

    reset <= '0';

    -- Currently set at 49.152 * 8 / 31 = 12.684MHz
    -- half VGA should be 25.175 / 2 = 12. 5875
    -- we could get closer with to cascaded multipliers
    Inst_DCM0 : DCM0
        port map (
            CLKIN_IN  => clock49,
            CLK0_OUT  => clock12,
            CLK0_OUT1 => open,
            CLK2X_OUT => open
            );

    Inst_DCMSID0 : DCMSID0
        port map (
            CLKIN_IN  => clock49,
            CLK0_OUT  => clock15,
            CLK0_OUT1 => open,
            CLK2X_OUT => open
            );
            
    Inst_DCMSID1 : DCMSID1
        port map (
            CLKIN_IN  => clock15,
            CLK0_OUT  => clock32,
            CLK0_OUT1 => open,
            CLK2X_OUT => open
            );            
            
    -- Motorola MC6847
    -- Original version: https://svn.pacedev.net/repos/pace/sw/src/component/video/mc6847.vhd
    -- Updated by AlanD for his Atom FPGA: http://stardot.org.uk/forums/viewtopic.php?f=3&t=6313
    -- A further few bugs fixed by myself
    Inst_mc6847 : mc6847
        port map (
            clk            => clock12,
            clk_ena        => '1',
            reset          => reset,
            da0            => open,
            videoaddr      => addrb,
            dd             => doutb,
            hs_n           => nHS,
            fs_n           => nFS,
            an_g           => ag_masked,
            an_s           => doutb(6),
            intn_ext       => doutb(6),
            gm             => gm_masked,
            css            => css_masked,
            inv            => doutb(7),
            red            => vga_red,
            green          => vga_green,
            blue           => vga_blue,
            hsync          => vga_hsync,
            vsync          => vga_vsync,
            artifact_en    => '0',
            artifact_set   => '0',
            artifact_phase => '0',
            hblank         => open,
            vblank         => open,
            cvbs           => open,
            black_backgnd  => BLACK_BACKGND
            );

    -- 8Kx8 Dual port video RAM
    -- Port A connects to Atom and is read/write
    -- Port B connects to MC6847 and is read only
    Inst_VideoRam : VideoRam
        port map (
            clka  => clock32,
            wea   => wr,
            addra => addra,
            dina  => dina,
            douta => douta,
            clkb  => clock12,
            web   => '0',
            addrb => addrb,
            dinb  => (others => '0'),
            doutb => doutb
            );

    Inst_sid6581: sid6581
        port map (
            clk_1MHz => clock1,
            clk32 => clock32,
            clk_DAC => clock49,
            reset => not nRST,
            cs => sid_cs,
            we => sid_we,
            addr => sid_addr,
            di => sid_di,
            do => sid_do,
            pot_x => '0',
            pot_y => '0',
            audio_out => sid_audio,
            audio_data => open 
        );


    -- Pipelined version of address/data/write signals
    process (clock32)
    begin
        if rising_edge(clock32) then
            nMS2 <= nMS1;
            nMS1 <= nMS;
            nWR2 <= nWR1;
            nWR1 <= nWR or nMS;
            DD2  <= DD1;
            DD1  <= DD;
            DA2  <= DA1;
            DA1  <= DA;
            div32 <= div32 + 1;
        end if;
    end process;

    -- Clock1 is derived by dividing clock32 down by 32
    clock1 <= div32(4);

    -- Signals driving the VRAM
    -- Write just before the rising edge of nWR
    wr    <= '1' when (nWR1 = '1' and nWR2 = '0') else '0';
    dina  <= DD2;
    addra <= DA2;
    
    -- Signals driving the SID
    -- Kees's Atom SID is at BDC0-BDDF
    -- This one will be at 9DC0-9DDF
    sid_cs <= '1' when nMS2 = '0' and DA2(12 downto 5) = "11101110" else '0';
    sid_we <= wr;
    sid_di <= DD2;
    sid_addr <= DA2(4 downto 0);
    
    -- Tri-state data back to the Atom
    dout <= sid_do when sid_cs = '1' else douta;
    DD    <= dout when (nMS = '0' and nWR = '1') else (others => 'Z');

    -- Unused PL4 Connectors
    -- Could also drive 1 bit RGB out here...
    OA  <= '0';
    OB  <= '0';
    CHB <= '0';
    Y   <= '0';

    -- RGB mapping
    R(1) <= vga_red(7);
    R(0) <= vga_red(6);
    G(0) <= vga_green(7);
    B(0) <= vga_blue(7);
    VSYNC <= vga_vsync;
    HSYNC <= vga_hsync;
    AUDIO <= sid_audio;

    -- Hold internal reset low for two frames after nRST released
    -- This avoids any diaplay glitches
    process (Clock12)
    variable state : std_logic_vector(2 downto 0);
    begin
        if rising_edge(Clock12) then
            if (nRST = '0') then
                state := "000";
            elsif (state = "000" and vga_vsync = '0') then
                state := "001";
            elsif (state = "001" and vga_vsync = '1') then
                state := "010";
            elsif (state = "010" and vga_vsync = '0') then
                state := "011";
            elsif (state = "011" and vga_vsync = '1') then
                state := "100";
            end if;
            mask <= state(2);
        end if;
    end process;
    
    -- During reset, force the 6847 mode select inputs low
    -- (this is necessary to stop the mode changing during reset, as the GODIL has 1.5K pullups)
    gm_masked  <= GM(2 downto 0) when mask = '1' else (others => '0');
    ag_masked  <= AG             when mask = '1' else '0';
    css_masked <= CSS            when mask = '1' else '0';

    -- Test Pins    
    Test1 <= '0';
    Test2 <= '0';
    Test3 <= '0';
    
end BEHAVIORAL;


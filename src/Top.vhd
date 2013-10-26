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
use IEEE.STD_LOGIC_1164.ALL;

-- Uncomment the following library declaration if using
-- arithmetic functions with Signed or Unsigned values
use IEEE.NUMERIC_STD.ALL;
use IEEE.STD_LOGIC_UNSIGNED.ALL;

-- Uncomment the following library declaration if instantiating
-- any Xilinx primitives in this code.
--library UNISIM;
--use UNISIM.VComponents.all;

entity Top is
    Port (

        -- Standard 6847 signals
        --
        -- expept DA which is now input only
        -- except nRP which re-purposed as a nWR
        -- except INV which is used as an output
        
        CLK : in  std_logic;
        DD : inout  std_logic_vector (7 downto 0);              
        DA : in  std_logic_vector (12 downto 0);
        CHB : out  std_logic;
        OA : out  std_logic;
        OB : out  std_logic;
        nMS : in  std_logic;
        CSS : in  std_logic;
        nHS : out  std_logic;
        nFS : out  std_logic;
        nWR : in  std_logic; -- Was nRP
        AG : in  std_logic;
        AS : in  std_logic;
        INV : in  std_logic;
        INTEXT : in  std_logic;
        GM : in  std_logic_vector (2 downto 0);
        Y : out  std_logic;
        
        -- 5 bit VGA Output
        
      R : out  std_logic_vector (1 downto 0);
      G: out  std_logic_vector (1 downto 0);
      B: out  std_logic_vector (0 downto 0);
      HSYNC : out  std_logic;
      VSYNC : out  std_logic;

        -- Other GODIL specific pins

        CLK49 : in  std_logic;
        
        -- Test Pins
        
        Test0 : out std_logic;
        Test1 : out std_logic;
        Test2 : out std_logic;
        Test3 : out std_logic
        

    );
end Top;

architecture BEHAVIORAL of Top is

  signal clock12 : std_logic;
  signal reset : std_logic;
  

    signal nWR1 : std_logic;
    signal nWR2 : std_logic;
    signal nMS1 : std_logic;
    signal nMS2 : std_logic;
    signal DA1 : std_logic_vector (12 downto 0);
    signal DA2 : std_logic_vector (12 downto 0);
    signal DD1 : std_logic_vector (7 downto 0);
    signal DD2 : std_logic_vector (7 downto 0);

    signal vred : std_logic_vector (7 downto 0);
    signal vgreen : std_logic_vector (7 downto 0);
    signal vblue : std_logic_vector (7 downto 0);
    signal wr : std_logic;

    signal addra : std_logic_vector (12 downto 0);
    signal dina : std_logic_vector (7 downto 0);
    signal douta : std_logic_vector (7 downto 0);
    
    signal addrb : std_logic_vector (12 downto 0);
    signal doutb : std_logic_vector (7 downto 0);
         
     component mc6847
    port(
        clk : in std_logic;
        clk_ena : in std_logic;
        reset : in std_logic;
        dd : in std_logic_vector(7 downto 0);
        an_g : in std_logic;
        an_s : in std_logic;
        intn_ext : in std_logic;
        gm : in std_logic_vector(2 downto 0);
        css : in std_logic;
        inv : in std_logic;
        artifact_en : in std_logic;
        artifact_set : in std_logic;
        artifact_phase : in std_logic;          
        da0 : out std_logic;
        videoaddr : out std_logic_vector(12 downto 0);
        hs_n : out std_logic;
        fs_n : out std_logic;
        red : out std_logic_vector(7 downto 0);
        green : out std_logic_vector(7 downto 0);
        blue : out std_logic_vector(7 downto 0);
        hsync : out std_logic;
        vsync : out std_logic;
        hblank : out std_logic;
        vblank : out std_logic;
        cvbs : out std_logic_vector(7 downto 0);
		  black_backgnd : in std_logic
        );
    end component;

    component VideoRam
      port (
         clka : in std_logic;
         wea : in std_logic;
         addra : in std_logic_vector(12 DOWNTO 0);
         dina : in std_logic_vector(7 DOWNTO 0);
         douta : out std_logic_vector(7 DOWNTO 0);
         clkb : in std_logic;
         web : in std_logic;
         addrb : in std_logic_vector(12 DOWNTO 0);
         dinb : in std_logic_vector(7 DOWNTO 0);
         doutb : out std_logic_vector(7 DOWNTO 0)
      );
    end component;

    component DCM0
    port(
        CLKIN_IN : in std_logic;          
        CLK0_OUT : out std_logic;
        CLK0_OUT1 : out std_logic;
        CLK2X_OUT : out std_logic
        );
    end component;


begin

    reset <= '0';

    Inst_DCM0: DCM0 PORT MAP(
        CLKIN_IN => CLK49,
        CLK0_OUT => clock12,
        CLK0_OUT1 => open,
        CLK2X_OUT => open
    );

    Inst_mc6847: mc6847 PORT MAP(
        clk => clock12,
        clk_ena => '1',
        reset => reset,
      da0 => open,
        videoaddr => addrb,
        dd => doutb,
        hs_n => nHS,
        fs_n => nFS,
        an_g => AG,
        an_s => doutb(6),
        intn_ext => doutb(6),
        gm => GM(2 downto 0),
        css => CSS,
        inv => doutb(7),
        red => vred,
        green => vgreen,
        blue => vblue,
        hsync => HSYNC,
        vsync => VSYNC,
        artifact_en => '0',
        artifact_set => '0',
        artifact_phase => '0',
        hblank => open,
        vblank => open,
        cvbs => open 
    );
    
    Inst_VideoRam : VideoRam
      port map (
         clka => CLK49,
         wea => wr,
         addra => addra,
         dina => dina,
         douta => douta,
         clkb => clock12,
         web => '0',
         addrb => addrb,
         dinb => (others => '0'),
         doutb => doutb
      );
      
     process (CLK49)
     begin
        if rising_edge(CLK49) then
              nWR2 <= nWR1;
              nWR1 <= nWR or nMS; 
             DD2 <= DD1;
             DD1 <= DD;
             DA2 <= DA1;
             DA1 <= DA;
         end if;
     end process;

     -- Write just before the rising edge of nWR
     -- (adds two cycles of latency to reads, bur this should be fine)
    wr <= '1' when (nWR1 = '1' and nWR2 = '0') else '0';     
     dina <= DD2;
    addra <= DA2;
     
     DD <= douta when (nMS = '0' and nWR = '1') else (others=>'Z');
          
     -- Unused PL4 Connectors
     -- Could also drive 1 bit RGB out here...

     OA <= '0';
     OB <= '0';
     CHB <= '0';
     Y  <= '0';

     -- RGB mapping
	  black_backgnd <= '0';
     R(1) <= vred(7);
     R(0) <= vred(6);
     G(1) <= vgreen(7);
     G(0) <= vgreen(6);
     B(0) <= vblue(7);
     
     -- Test Pins
     
     Test0 <= clock12;
     Test1 <= CLK49;
     Test2 <= '0';
     Test3 <= '0';
    
end BEHAVIORAL;


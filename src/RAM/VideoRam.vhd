library ieee;
use ieee.std_logic_1164.all;
use ieee.std_logic_unsigned.all;

entity VideoRam is

   port ( 
		 clka : IN STD_LOGIC;
		 wea : IN STD_LOGIC;
		 addra : IN STD_LOGIC_VECTOR(12 DOWNTO 0);
		 dina : IN STD_LOGIC_VECTOR(7 DOWNTO 0);
		 douta : OUT STD_LOGIC_VECTOR(7 DOWNTO 0);
		 clkb : IN STD_LOGIC;
		 web : IN STD_LOGIC;
		 addrb : IN STD_LOGIC_VECTOR(12 DOWNTO 0);
		 dinb : IN STD_LOGIC_VECTOR(7 DOWNTO 0);
		 doutb : OUT STD_LOGIC_VECTOR(7 DOWNTO 0)
	);
	end VideoRam;

architecture BEHAVIORAL of VideoRam is

-- Shared memory
type ram_type is array (8191 downto 0) of std_logic_vector (7 downto 0);
shared variable RAM : ram_type;

--attribute RAM_STYLE : string;
--attribute RAM_STYLE of RAM: signal is "BLOCK";

begin

	process (clka)
	begin
		if rising_edge(clka) then
			if (wea = '1' ) then
					RAM(conv_integer(addra(12 downto 0))) := dina;
			end if;
			douta <= RAM(conv_integer(addra(12 downto 0))); 	   
		end if;	 
	end process;	

	process (clkb)
	begin
		if rising_edge(clkb) then
			if (web = '1' ) then
					RAM(conv_integer(addrb(12 downto 0))) := dinb;
			end if;
			doutb <= RAM(conv_integer(addrb(12 downto 0))); 	   
		end if;	 
	end process;	

end BEHAVIORAL;




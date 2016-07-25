kinds
trailerKinds
	" see class comment for description. If you change this method, evaluate this:
	self generateTrailerKindDecoders"
	
	^#(
"000000" #NoTrailer
"000001" #ClearedTrailer
"000010" #TempsNamesQCompress
"000011" #TempsNamesZip
"000100" #SourceBySelector
"000101" #SourceByStringIdentifier
"000110" #EmbeddedSourceQCompress
"000111" #EmbeddedSourceZip
"001000" #VarLengthSourcePointer
"001001" #ExtendedKind
"001010" #NativeCodeTrailer  "this number is hardcoded in VM"
"001011" #EmbeddedSource
"001100" #Undefined
"001101" #Undefined
"001110" #Undefined
"001111" #Undefined
"010000" #Undefined
"010001" #Undefined
"010010" #Undefined
"010011" #Undefined
"010100" #Undefined
"010101" #Undefined
"010110" #Undefined
"010111" #Undefined
"011000" #Undefined
"011001" #Undefined
"011010" #Undefined
"011011" #Undefined
"011100" #Undefined
"011101" #Undefined
"011110" #Undefined
"011111" #Undefined
"100000" #Undefined
"100001" #Undefined
"100010" #Undefined
"100011" #Undefined
"100100" #Undefined
"100101" #Undefined
"100110" #Undefined
"100111" #Undefined
"101000" #Undefined
"101001" #Undefined
"101010" #Undefined
"101011" #Undefined
"101100" #Undefined
"101101" #Undefined
"101110" #Undefined
"101111" #Undefined
"110000" #Undefined
"110001" #Undefined
"110010" #Undefined
"110011" #Undefined
"110100" #Undefined
"110101" #Undefined
"110110" #Undefined
"110111" #Undefined
"111000" #Undefined
"111001" #Undefined
"111010" #Undefined
"111011" #Undefined
"111100" #Undefined
"111101" #Undefined
"111110" #Undefined
"111111" #SourcePointer
	)
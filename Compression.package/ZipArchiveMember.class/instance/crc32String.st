accessing
crc32String
	| hexString |
	hexString := crc32 storeStringHex.
	^('00000000' copyFrom: 1 to: 11 - (hexString size)) , (hexString copyFrom: 4 to: hexString size)
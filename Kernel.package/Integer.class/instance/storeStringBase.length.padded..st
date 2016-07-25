printing-numerative
storeStringBase: base length: minimum padded: zeroFlag
	^String streamContents: [:s| 
		self storeOn: s base: base length: minimum padded: zeroFlag ]
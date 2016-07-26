printing-numerative
printStringBase: base length: minimum padded: zeroFlag
	^String streamContents: [:s| self printOn: s base: base length: minimum padded: zeroFlag]
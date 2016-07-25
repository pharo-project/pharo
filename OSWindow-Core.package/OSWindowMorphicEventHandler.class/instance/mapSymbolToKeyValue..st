visiting
mapSymbolToKeyValue: symbol
	^ SymbolCharacterMapping at: symbol ifAbsent: [
		 "Don't allow symbol values outside the unicode range"
		 symbol >= 16r400000 ifTrue: [ 0 ] ifFalse: [ symbol ] ]
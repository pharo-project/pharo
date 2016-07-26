private-scanning
scanAnySymbol
	characterType = #alphabetic ifTrue: [^self scanSymbol].
	characterType = #binary ifTrue: [^self scanBinary: RBLiteralToken].
	^RBToken new
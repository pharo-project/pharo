inflating
proceedStoredBlock
	"Proceed decompressing a stored (e.g., uncompressed) block"
	| length decoded |
	"Literal table must be nil for a stored block"
	litTable == nil ifFalse:[^self error:'Bad state'].
	length := distTable.
	[length > 0 and:[readLimit < collection size and:[sourcePos < sourceLimit]]] 
		whileTrue:[
			collection at: (readLimit := readLimit + 1) put: 
				(source at: (sourcePos := sourcePos + 1)).
			length := length - 1].
	length = 0 ifTrue:[state := state bitAnd: StateNoMoreData].
	decoded := length - distTable.
	distTable := length.
	^decoded
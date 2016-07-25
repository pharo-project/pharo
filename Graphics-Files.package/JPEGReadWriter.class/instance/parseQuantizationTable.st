marker parsing
parseQuantizationTable
	| length markerStart n prec value table |
	markerStart := self position.
	length := self nextWord.
	[ self position - markerStart >= length ] whileFalse: 
		[ value := self next.
		n := (value bitAnd: 15) + 1.
		prec := value >> 4 > 0.
		n > QuantizationTableSize ifTrue: 
			[ self error: 'image has more than ' , QuantizationTableSize printString , ' quantization tables' ].
		table := IntegerArray new: DCTSize2.
		1 
			to: DCTSize2
			do: 
				[ :i | 
				value := prec 
					ifTrue: [ self nextWord ]
					ifFalse: [ self next ].
				table 
					at: (JPEGNaturalOrder at: i)
					put: value ].
		self useFloatingPoint ifTrue: [ self scaleQuantizationTable: table ].
		self qTable 
			at: n
			put: table ]
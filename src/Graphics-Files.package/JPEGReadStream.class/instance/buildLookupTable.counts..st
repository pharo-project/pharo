huffman trees
buildLookupTable: values counts: counts 
	| min max |
	min := max := nil.
	1 
		to: counts size
		do: 
			[ :i | 
			(counts at: i) = 0 ifFalse: 
				[ min ifNil: [ min := i - 1 ].
				max := i ] ].
	^ self 
		createHuffmanTables: values
		counts: {  0  } , counts
		from: min + 1
		to: max
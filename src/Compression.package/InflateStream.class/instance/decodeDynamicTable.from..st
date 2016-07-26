huffman trees
decodeDynamicTable: nItems from: aHuffmanTable
	"Decode the code length of the literal/length and distance table
	in a block compressed with dynamic huffman trees"
	| values index value repCount theValue |
	values := Array new: nItems.
	index := 1.
	theValue := 0.
	[index <= nItems] whileTrue:[
		value := self decodeValueFrom: aHuffmanTable.
		value < 16 ifTrue:[
			"Immediate values"
			theValue := value.
			values at: index put: value.
			index := index+1.
		] ifFalse:[
			"Repeated values"
			value = 16 ifTrue:[
				"Repeat last value"
				repCount := (self nextBits: 2) + 3.
			] ifFalse:[
				"Repeat zero value"
				theValue := 0.
				value = 17 
					ifTrue:[repCount := (self nextBits: 3) + 3]
					ifFalse:[value = 18 
								ifTrue:[repCount := (self nextBits: 7) + 11]
								ifFalse:[^self error:'Invalid bits tree value']]].
			0 to: repCount-1 do:[:i| values at: index+i put: theValue].
			index := index + repCount].
	].
	^values
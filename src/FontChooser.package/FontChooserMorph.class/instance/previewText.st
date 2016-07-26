accessing
previewText
	"Answer the preview text based on current font."
	
	| sample i maxLineLength endOfLineCharacters |
	model selectedFont isNil ifTrue: [ ^'' ].
	sample := String new writeStream.
	model selectedFont isSymbolFont ifFalse: [ 
		| pangram |
		pangram := self selectedPangram.
		sample 
			nextPutAll: pangram; cr;
			nextPutAll: pangram asUppercase;
			cr ].
	i := 0.
	maxLineLength := 30.
	endOfLineCharacters := '@Z`z'.
	33 to: 255 do: [ :asciiValue | | character |
		character := Character value: asciiValue.
		sample nextPut: character.
		i := i + 1.
		((endOfLineCharacters includes: character) or: [ i = maxLineLength ]) 
			ifTrue: [ 
				i := 0.
				sample cr ] ].
	^ sample contents.
converting
nextCodePointFromStream: stream
	"In non-strict mode, we let byte values for holes in our mapping pass through"

	| byteValue |
	^ (byteValue := stream next) < 128
		ifTrue: [ byteValue ]
		ifFalse: [ 
			(byteToUnicode at: byteValue - 127 ifAbsent: [ nil ])
				ifNotNil: [ :unicode | unicode asInteger ]
				ifNil: [ 
					strict
						ifTrue: [ self errorOutsideRange ]
						ifFalse: [ byteValue ] ] ]
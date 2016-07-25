converting
nextFromStream: stream
	"In non-strict mode, we let byte values for holes in our mapping pass through.
	Overwritten for performance reasons"

	| byteValue |
	^ (byteValue := stream next) < 128
		ifTrue: [ Character value: byteValue ]
		ifFalse: [ 
			(byteToUnicode at: byteValue - 127 ifAbsent: [ nil ])
				ifNotNil: [ :unicode | unicode ]
				ifNil: [ 
					strict
						ifTrue: [ self errorOutsideRange ]
						ifFalse: [ Character value: byteValue ] ] ]
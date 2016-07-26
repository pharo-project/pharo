printing
printOn: aStream

	| beforeFirst | 
	beforeFirst := true.
	self do:
		[:each |
		beforeFirst
			ifTrue: [beforeFirst := false]
			ifFalse: [
				each isString
					ifTrue: [ aStream nextPut: $- ]
					ifFalse: [ aStream nextPut: $. ] ].
		aStream nextPutAll: each asString ]
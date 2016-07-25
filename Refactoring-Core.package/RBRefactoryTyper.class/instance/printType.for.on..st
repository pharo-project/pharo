printing
printType: aClass for: aString on: aStream 
	| name colTypes |
	colTypes := #().
	name := self collectionNameFor: aString.
	(aClass includesClass: (model classFor: Collection))
		ifTrue: [colTypes := self guessTypesFor: name].
	colTypes isEmpty ifFalse: [aStream nextPut: $(].
	aClass printOn: aStream.
	colTypes isEmpty
		ifFalse: 
			[aStream nextPutAll: ' of: '.
			colTypes size > 1 ifTrue: [aStream nextPut: $(].
			self printTypeFor: name on: aStream.
			colTypes size > 1 ifTrue: [aStream nextPut: $)]].
	colTypes isEmpty ifFalse: [aStream nextPut: $)]
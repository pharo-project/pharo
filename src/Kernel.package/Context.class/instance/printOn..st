printing
printOn: aStream
	self outerContext
		ifNil:
			[ | selector class mclass |
			self method == nil
				ifTrue: [ ^ super printOn: aStream ].
			class := self receiver class.
			mclass := self methodClass.
			selector := self selector ifNil: [ self method defaultSelector ].
			aStream nextPutAll: class name.
			mclass == class
				ifFalse:
					[ aStream nextPut: $(.
					aStream nextPutAll: mclass name.
					aStream nextPut: $) ].
			aStream nextPutAll: '>>'.
			aStream nextPutAll: selector.
			selector = #doesNotUnderstand:
				ifTrue:
					[ aStream space.
					(self tempAt: 1) selector printOn: aStream ] ]
		ifNotNil:
			[ :outerContext | 
			aStream nextPutAll: closureOrNil printString , ' in '.
			outerContext printOn: aStream ]
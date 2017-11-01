Extension { #name : #ByteSymbol }

{ #category : #'*Fuel' }
ByteSymbol >> fuelAccept: aGeneralMapper [

	^aGeneralMapper visitHookPrimitive: self
]

{ #category : #'*Fuel' }
ByteSymbol class >> materializeFrom: aDecoder [

	^ aDecoder nextEncodedString asSymbol
]

{ #category : #'*Fuel' }
ByteSymbol >> serializeOn: anEncoder [

	anEncoder encodeString: self
]

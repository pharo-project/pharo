Extension { #name : #ByteSymbol }

{ #category : #'*Fuel-Core' }
ByteSymbol >> fuelAccept: aGeneralMapper [

	^aGeneralMapper visitHookPrimitive: self
]

{ #category : #'*Fuel-Core' }
ByteSymbol class >> materializeFrom: aDecoder [

	^ aDecoder nextEncodedString asSymbol
]

{ #category : #'*Fuel-Core' }
ByteSymbol >> serializeOn: anEncoder [

	anEncoder encodeString: self
]

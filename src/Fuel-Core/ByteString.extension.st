Extension { #name : #ByteString }

{ #category : #'*Fuel-Core' }
ByteString >> fuelAccept: aGeneralMapper [

	^aGeneralMapper visitHookPrimitive: self
]

{ #category : #'*Fuel-Core' }
ByteString class >> materializeFrom: aDecoder [

	^ aDecoder nextEncodedString
]

{ #category : #'*Fuel-Core' }
ByteString >> serializeOn: anEncoder [

	anEncoder encodeString: self
]

Extension { #name : #ByteString }

{ #category : #'*Fuel' }
ByteString >> fuelAccept: aGeneralMapper [

	^aGeneralMapper visitHookPrimitive: self
]

{ #category : #'*Fuel' }
ByteString class >> materializeFrom: aDecoder [

	^ aDecoder nextEncodedString
]

{ #category : #'*Fuel' }
ByteString >> serializeOn: anEncoder [

	anEncoder encodeString: self
]

Extension { #name : #Character }

{ #category : #'*Fuel' }
Character >> fuelAccept: aGeneralMapper [

	^aGeneralMapper visitHookPrimitive: self
]

{ #category : #'*Fuel' }
Character >> largeIdentityHash [

	^ self identityHash
]

{ #category : #'*Fuel' }
Character class >> materializeFrom: aDecoder [

	^self value: aDecoder nextEncodedUint32
]

{ #category : #'*Fuel' }
Character >> serializeOn: anEncoder [

	anEncoder encodeUint32: self asInteger
]

Extension { #name : #Character }

{ #category : #'*Fuel-Core' }
Character >> fuelAccept: aGeneralMapper [

	^aGeneralMapper visitHookPrimitive: self
]

{ #category : #'*Fuel-Core' }
Character >> largeIdentityHash [

	^ self identityHash
]

{ #category : #'*Fuel-Core' }
Character class >> materializeFrom: aDecoder [

	^self value: aDecoder nextEncodedUint32
]

{ #category : #'*Fuel-Core' }
Character >> serializeOn: anEncoder [

	anEncoder encodeUint32: self asInteger
]

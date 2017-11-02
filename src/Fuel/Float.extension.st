Extension { #name : #Float }

{ #category : #'*Fuel' }
Float >> fuelAccept: aGeneralMapper [

	^aGeneralMapper visitHookPrimitive: self
]

{ #category : #'*Fuel' }
Float class >> materializeFrom: aDecoder [

	^ (self new: 2)
		at: 1 put: aDecoder nextEncodedUint32;
		at: 2 put: aDecoder nextEncodedUint32;
		yourself.

]

{ #category : #'*Fuel' }
Float >> serializeOn: anEncoder [

	anEncoder 
		encodeUint32: (self at: 1);
		encodeUint32: (self at: 2).
]

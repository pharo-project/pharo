Extension { #name : #Float }

{ #category : #'*Fuel' }
Float >> fuelAccept: aGeneralMapper [

	^aGeneralMapper visitHookPrimitive: self
]

{ #category : #'*Fuel' }
Float class >> materializeFrom: aDecoder [
	"The * 1 is here to give a chance to the VM to create a SmallFloat64 if it can (thanks Nicolas Cellier)."
	
	^ (BoxedFloat64 new: 2)
		at: 1 put: aDecoder nextEncodedUint32;
		at: 2 put: aDecoder nextEncodedUint32;
		* 1.

]

{ #category : #'*Fuel' }
Float >> serializeOn: anEncoder [

	anEncoder 
		encodeUint32: (self at: 1);
		encodeUint32: (self at: 2).
]

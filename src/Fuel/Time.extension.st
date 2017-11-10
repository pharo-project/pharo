Extension { #name : #Time }

{ #category : #'*Fuel' }
Time >> fuelAccept: aGeneralMapper [

	^ aGeneralMapper visitHookPrimitive: self

]

{ #category : #'*Fuel' }
Time class >> materializeFrom: aDecoder [
	
	^ self 
		seconds: aDecoder nextEncodedUint24
		nanoSeconds: aDecoder nextEncodedUint32
]

{ #category : #'*Fuel' }
Time >> serializeOn: anEncoder [

	anEncoder 
		encodeUint24: self asSeconds;
		encodeUint32: self nanoSecond.
		
]

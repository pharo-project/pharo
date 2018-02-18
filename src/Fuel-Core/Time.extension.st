Extension { #name : #Time }

{ #category : #'*Fuel-Core' }
Time >> fuelAccept: aGeneralMapper [

	^ aGeneralMapper visitHookPrimitive: self

]

{ #category : #'*Fuel-Core' }
Time class >> materializeFrom: aDecoder [
	
	^ self 
		seconds: aDecoder nextEncodedUint24
		nanoSeconds: aDecoder nextEncodedUint32
]

{ #category : #'*Fuel-Core' }
Time >> serializeOn: anEncoder [

	anEncoder 
		encodeUint24: self asSeconds;
		encodeUint32: self nanoSecond.
		
]

Extension { #name : #Date }

{ #category : #'*Fuel-Core' }
Date >> fuelAccept: aGeneralMapper [

	^ aGeneralMapper visitHookPrimitive: self

]

{ #category : #'*Fuel-Core' }
Date class >> materializeFrom: aDecoder [
	^ self 
		starting: (DateAndTime materializeFrom: aDecoder) midnight 
		duration: (Duration days: 1)
]

{ #category : #'*Fuel-Core' }
Date >> serializeOn: anEncoder [
	start serializeOn: anEncoder
]

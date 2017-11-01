Extension { #name : #Date }

{ #category : #'*Fuel' }
Date >> fuelAccept: aGeneralMapper [

	^ aGeneralMapper visitHookPrimitive: self

]

{ #category : #'*Fuel' }
Date class >> materializeFrom: aDecoder [
	^ self 
		starting: (DateAndTime materializeFrom: aDecoder) midnight 
		duration: (Duration days: 1)
]

{ #category : #'*Fuel' }
Date >> serializeOn: anEncoder [
	start serializeOn: anEncoder
]

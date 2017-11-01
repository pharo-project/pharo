Extension { #name : #Boolean }

{ #category : #'*Fuel' }
Boolean >> fuelAccept: aGeneralMapper [

	^aGeneralMapper visitHookPrimitive: self
]

{ #category : #'*Fuel' }
Boolean >> serializeOn: anEncoder [
	"Do nothing"
]

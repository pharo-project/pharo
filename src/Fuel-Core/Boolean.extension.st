Extension { #name : #Boolean }

{ #category : #'*Fuel-Core' }
Boolean >> fuelAccept: aGeneralMapper [

	^aGeneralMapper visitHookPrimitive: self
]

{ #category : #'*Fuel-Core' }
Boolean >> serializeOn: anEncoder [
	"Do nothing"
]

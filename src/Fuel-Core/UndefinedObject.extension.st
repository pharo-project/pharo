Extension { #name : #UndefinedObject }

{ #category : #'*Fuel-Core' }
UndefinedObject >> fuelAccept: aGeneralMapper [

	^aGeneralMapper visitHookPrimitive: self
]

{ #category : #'*Fuel-Core' }
UndefinedObject class >> materializeFrom: aDecoder [
	"Answer my unique instance"

	^ nil
]

{ #category : #'*Fuel-Core' }
UndefinedObject >> serializeOn: anEncoder [
	"Do nothing"
]

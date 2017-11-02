Extension { #name : #UndefinedObject }

{ #category : #'*Fuel' }
UndefinedObject >> fuelAccept: aGeneralMapper [

	^aGeneralMapper visitHookPrimitive: self
]

{ #category : #'*Fuel' }
UndefinedObject class >> materializeFrom: aDecoder [
	"Answer my unique instance"

	^ nil
]

{ #category : #'*Fuel' }
UndefinedObject >> serializeOn: anEncoder [
	"Do nothing"
]

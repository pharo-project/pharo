Extension { #name : #TraitBehavior }

{ #category : #'*Fuel' }
TraitBehavior >> fuelIgnoredInstanceVariableNames [
	"Indicates which variables have to be ignored during serialization."

	^#()
]

{ #category : #'*Fuel' }
TraitBehavior >> fuelNew [
	"Answer an instance of mine in which serialized references will be injected."

	^ self basicNew
]

{ #category : #'*Fuel' }
TraitBehavior >> fuelNew: sizeRequested [ 
	"Answer an instance of mine in which serialized references will be injected."

	^ self basicNew: sizeRequested
]

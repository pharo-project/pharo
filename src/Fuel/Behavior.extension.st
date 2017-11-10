Extension { #name : #Behavior }

{ #category : #'*Fuel' }
Behavior >> fuelIgnoredInstanceVariableNames [
	"Indicates which variables have to be ignored during serialization."

	^#()
]

{ #category : #'*Fuel' }
Behavior >> fuelNew [
	"Answer an instance of mine in which serialized references will be injected."

	^ self basicNew
]

{ #category : #'*Fuel' }
Behavior >> fuelNew: sizeRequested [ 
	"Answer an instance of mine in which serialized references will be injected."

	^ self basicNew: sizeRequested
]

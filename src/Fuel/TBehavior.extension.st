Extension { #name : #TBehavior }

{ #category : #'*Fuel' }
TBehavior >> fuelIgnoredInstanceVariableNames [
	"Indicates which variables have to be ignored during serialization."

	^#()
]

{ #category : #'*Fuel' }
TBehavior >> fuelNew [
	"Answer an instance of mine in which serialized references will be injected."

	^ self basicNew
]

{ #category : #'*Fuel' }
TBehavior >> fuelNew: sizeRequested [ 
	"Answer an instance of mine in which serialized references will be injected."

	^ self basicNew: sizeRequested
]

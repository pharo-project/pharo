Class {
	#name : #SetElement,
	#superclass : #CollectionElement,
	#category : #Deprecated80
}

{ #category : #testing }
SetElement class >> isDeprecated [
	"Use CollectionElementWrapper instead."

	^ true
]

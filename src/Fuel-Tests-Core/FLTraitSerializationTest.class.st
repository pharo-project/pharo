"
I have the common behavior for testing trait serialization.
"
Class {
	#name : #FLTraitSerializationTest,
	#superclass : #FLSerializationTest,
	#category : #'Fuel-Tests-Core-Base'
}

{ #category : #running }
FLTraitSerializationTest >> newClassOrTrait [
	"Returns a trait for testing"

	^ self newTraitSuffixed: 'Main'
]

{ #category : #running }
FLTraitSerializationTest >> newInstanceFrom: aTrait [

	| aClass |
	aClass := self newClass.
	"If one wants to use a materialized trait in a class, it has to be added to Smalltalk globals. Fuel does not do that. Fuel just materializes the trait. It is up to the user to decide what to do with it.  "
	testingEnvironment at: aTrait name put: aTrait.
	aClass addToComposition: aTrait.
	^ aClass new
]

{ #category : #running }
FLTraitSerializationTest >> newSecondaryTrait [
	"Returns a trait for testing"

	^ self newTraitSuffixed: 'Secondary'
]

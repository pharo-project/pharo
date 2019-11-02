"
I have the common behavior for testing class serialization.
"
Class {
	#name : #FLClassSerializationTest,
	#superclass : #FLSerializationTest,
	#category : #'Fuel-Tests-Core-Base'
}

{ #category : #tests }
FLClassSerializationTest >> during: aBlock rename: aClass as: anotherClass [

	[ testingEnvironment at: aClass name put: anotherClass.
	^aBlock value ] ensure: [ 
	testingEnvironment at: aClass name put: aClass ].
]

{ #category : #tests }
FLClassSerializationTest >> newClassOrTrait [
	"Returns a class for testing"

	^ self newClassWithInstanceVariableNames: ''
]

{ #category : #tests }
FLClassSerializationTest >> newClassOrTraitWithSuperClass: superclass [
	"Returns a class for testing"

	^ self newClassWithInstanceVariableNames: '' superclass: superclass
]

{ #category : #running }
FLClassSerializationTest >> newClassWithInstanceVariableNames: instanceVariableNames [
	"Returns a class for testing, with the specified instance variables."

	^ self newClassWithInstanceVariableNames: instanceVariableNames superclass: Object
]

{ #category : #running }
FLClassSerializationTest >> newClassWithInstanceVariableNames: instanceVariableNames superclass: aSuperclass [
	"Returns a class for testing, with the specified instance variables."

	^ self 
		newSubclassOf: aSuperclass
		instanceVariableNames: instanceVariableNames
		classVariableNames: ''
]

{ #category : #tests }
FLClassSerializationTest >> newInstanceFrom: aClass [

	^ aClass new
]

{ #category : #running }
FLClassSerializationTest >> newSecondaryTrait [
	"Returns a trait for testing"

	^ self newTraitSuffixed: 'Secondary'
]

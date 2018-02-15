"
A test mock for a class with recursive substitution
"
Class {
	#name : #FLClassWithRecursiveSubstitution,
	#superclass : #Object,
	#instVars : [
		'index'
	],
	#category : #'Fuel-Tests-Mocks'
}

{ #category : #serialization }
FLClassWithRecursiveSubstitution >> fuelAccept: aGeneralMapper [

	^ index < 10
		ifTrue: [
			aGeneralMapper 
				visitSubstitution: self 
				by: (self copy index: self index + 1)
				onRecursionDo: [ super fuelAccept: aGeneralMapper ] ]
		ifFalse: [
			super fuelAccept: aGeneralMapper ]
]

{ #category : #accessing }
FLClassWithRecursiveSubstitution >> index [
	^ index
]

{ #category : #accessing }
FLClassWithRecursiveSubstitution >> index: anObject [
	index := anObject 
]

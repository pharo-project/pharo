"
A class instance variable definition
"
Class {
	#name : #RGClassInstanceVariableDefinition,
	#superclass : #RGVariableDefinition,
	#category : #'Ring-Definitions-Core-Base'
}

{ #category : #initialization }
RGClassInstanceVariableDefinition >> initialize [ 

	super initialize.
	self isMetaSide: true.
]

{ #category : #testing }
RGClassInstanceVariableDefinition >> isClassInstanceVariable [

	^true
]

"
I am example class which has methods with halts
"
Class {
	#name : #ClyClassWithHalts,
	#superclass : #Object,
	#category : #'Calypso-SystemPlugins-Reflectivity-Queries-Tests-Breakpoints'
}

{ #category : #methods }
ClyClassWithHalts >> methodWithHalts1 [
	self halt
]

{ #category : #methods }
ClyClassWithHalts >> methodWithHalts2 [
	self halt
]

{ #category : #methods }
ClyClassWithHalts >> methodWithoutHalts [

	self printString
]

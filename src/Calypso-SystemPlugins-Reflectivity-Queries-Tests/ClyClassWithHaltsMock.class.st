"
I am example class which has methods with halts
"
Class {
	#name : #ClyClassWithHaltsMock,
	#superclass : #Object,
	#category : #'Calypso-SystemPlugins-Reflectivity-Queries-Tests-Breakpoints'
}

{ #category : #methods }
ClyClassWithHaltsMock >> methodWithoutHalts [

	self printString
]

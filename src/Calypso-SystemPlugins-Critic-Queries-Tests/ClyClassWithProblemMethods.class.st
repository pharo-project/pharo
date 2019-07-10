"
I am an example class which methods have example critiques
"
Class {
	#name : #ClyClassWithProblemMethods,
	#superclass : #Object,
	#category : #'Calypso-SystemPlugins-Critic-Queries-Tests'
}

{ #category : #'method examples' }
ClyClassWithProblemMethods >> methodWithHalt [
	<haltOrBreakpointForTesting>
	self halt.
]

{ #category : #'method examples' }
ClyClassWithProblemMethods >> methodWithHalt2 [
	self halt.
]

{ #category : #'method examples' }
ClyClassWithProblemMethods >> methodWithoutCritiques [
	"this is example method which should not have critiques"
	^self printString
]

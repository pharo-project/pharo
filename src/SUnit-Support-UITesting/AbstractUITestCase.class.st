"
This is an abstract superclass for all tests requiring a user interface (UI). These tests are skipped when Pharo runs in headless mode
"
Class {
	#name : #AbstractUITestCase,
	#superclass : #TestCase,
	#category : #'SUnit-Support-UITesting-Base'
}

{ #category : #testing }
AbstractUITestCase class >> isAbstract [

	^self == AbstractUITestCase
]

{ #category : #running }
AbstractUITestCase >> runCase [
	| result |
	"Skip the test if we're in headless mode"
	self flag: 'Use skip feature once it is ready'.
	Smalltalk isHeadless
		ifTrue: [ ^ self skip ].
	SystemProgressMorph allInstances do: [  :spm | spm hide ].
	self currentWorld activeHand resetClickState.
	result := super runCase .
	SystemProgressMorph allInstances do: [  :spm | spm show ].
	^ result
]

"
All instances of UITestCase are skipped in headless mode.
"
Class {
	#name : #UITestCase,
	#superclass : #TestCase,
	#category : #'SUnit-Support-UITesting'
}

{ #category : #running }
UITestCase >> runCase [
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

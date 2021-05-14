"
I am only there to be used by the StBreakpointCacheTest class.
Compile method with and without breakpoints, but remove them after the tests.
"
Class {
	#name : #StDummyTestClassWithHalts,
	#superclass : #Object,
	#instVars : [
		'var'
	],
	#category : #'NewTools-Debugger-Breakpoints-Tools-Tests'
}

{ #category : #accessing }
StDummyTestClassWithHalts >> var [
	"the var is used for testing, this accessor us just here so that it has one user"
	^var
]

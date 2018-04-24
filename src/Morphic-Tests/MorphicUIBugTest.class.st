"
A MorphicUIBugTest is a class for testing the shortcomings and repairs of the MorphicUI manager.
.

Instance Variables
	cases:		<aCollection>

cases
	- a list of morphs that may need to be deleted during teardown.
	the tests are expected to fill this list it starts out empty by default.
	
	

"
Class {
	#name : #MorphicUIBugTest,
	#superclass : #TestCase,
	#instVars : [
		'cases'
	],
	#category : #'Morphic-Tests-Bugs'
}

{ #category : #running }
MorphicUIBugTest >> setUp [
	"default. tests will add morphs to list. Teardown will delete."
	super setUp.
	cases := #()
]

{ #category : #running }
MorphicUIBugTest >> tearDown [
	"default. tests will add morphs to list. Teardown will delete."

	cases do: [ :each | each delete ].
	super tearDown
]

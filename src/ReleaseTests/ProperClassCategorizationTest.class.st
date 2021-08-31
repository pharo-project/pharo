"
Tests to enforce proper categorization of classes
"
Class {
	#name : #ProperClassCategorizationTest,
	#superclass : #TestCase,
	#category : #'ReleaseTests-Categorization'
}

{ #category : #tests }
ProperClassCategorizationTest >> testProperClassTagCasing [
	|violations|
	violations := OrderedCollection new.
	Smalltalk packages
        do: [:package | package classTags do: [:classTag | classTag name first isLowercase ifTrue: [ violations add: (package -> classTag) ] ] ].
	self assert:  violations isEmpty description: 'Class Tags should be uppercase'
]

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
	RPackageOrganizer default packages
        do: [:each | each classTags do: [:ct | ct name first isLowercase ifTrue: [ violations add: (each -> ct) ] ] ].
	self assert:  violations isEmpty description: 'Class Tags should be uppercase'
]

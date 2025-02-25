"
A RubEditingStateTest is a test class for testing the behavior of RubEditingState
"
Class {
	#name : 'RubEditingStateTest',
	#superclass : 'TestCase',
	#category : 'Rubric-Tests-Editing-Core',
	#package : 'Rubric-Tests',
	#tag : 'Editing-Core'
}

{ #category : 'tests' }
RubEditingStateTest >> testSelectionStart [

	| m |
	"Create a RubTextFieldArea which calls the tested method"
	m := RubTextFieldArea new
		     width: 300;
		     maxLength: 30;
		     yourself.
	self assert: m string isEmpty.
	m privateReplaceFrom: 1 to: 0 with: 'one two three'.

	m selectFrom: 1 to: 3.
	self assert: m selection asString equals: 'one'.
	self assert: m selectionStart stringIndex equals: 1.

	m selectFrom: 4 to: 0.
	self assert: m selection asString equals: 'one'.
	self assert: m selectionStart stringIndex equals: 1.

	m selectFrom: -10 to: 1000.
	self assert: m selection asString equals: 'one two three'.
	self assert: m selectionStart stringIndex equals: 1.

	m selectFrom: 100 to: -1000.
	self assert: m selection asString equals: 'one two three'.
	self assert: m selectionStart stringIndex equals: 1
]

{ #category : 'tests' }
RubEditingStateTest >> testSelectionStop [

	| m |
	"Create a RubTextFieldArea which calls the tested method"
	m := RubTextFieldArea new
		     width: 300;
		     maxLength: 30;
		     yourself.
	self assert: m string isEmpty.
	m privateReplaceFrom: 1 to: 0 with: 'one two three'.

	m selectFrom: 0 to: 3.
	self assert: m selection asString equals: 'one'.
	self assert: m selectionStop stringIndex equals: 4.

	m selectFrom: 4 to: 0.
	self assert: m selection asString equals: 'one'.
	self assert: m selectionStop stringIndex equals: 4.

	m selectFrom: 0 to: 1000.
	self assert: m selection asString equals: 'one two three'.
	self assert: m selectionStop stringIndex equals: 14.

	m selectFrom: 1000 to: 0.
	self assert: m selection asString equals: 'one two three'.
	self assert: m selectionStop stringIndex equals: 14
]

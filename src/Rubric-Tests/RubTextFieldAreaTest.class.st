Class {
	#name : #RubTextFieldAreaTest,
	#superclass : #TestCase,
	#category : #'Rubric-Tests-Editing-Core'
}

{ #category : #tests }
RubTextFieldAreaTest >> testMaxLengthIsNegative [
	| m |
	m := RubTextFieldArea new
		width: 300;
		maxLength: -10;
		yourself.
	m setTextWith: '01234567890123456'.
	self assert: m text size equals: 0.

]

{ #category : #tests }
RubTextFieldAreaTest >> testMaxLengthIsSet [
	| m |
	m := RubTextFieldArea new
		width: 300;
		maxLength: 15;
		yourself.
	self assert: m maxLength equals: 15
]

{ #category : #tests }
RubTextFieldAreaTest >> testMaxLengthTruncatesText [
	| m |
	m := RubTextFieldArea new
		width: 300;
		maxLength: 15;
		yourself.
	m setTextWith: '01234567890123456'.
	" the text should be truncated "
	self assert: m text size equals: 15.
	
	" the text is different to ensure that the input is validate and update is sent instead of set "
	m updateTextWith: '7890123456789012345'.
	self assert: m text size equals: 15.
	
	" with a text of size less than maxlength  to check that maxLength has no effect "
	m setTextWith: ''.
	self assert: m text size equals: 0.	
	m setTextWith: 'A'.
	self assert: m text size equals: 1.
	m updateTextWith: ''.
	self assert: m text size equals: 0
]

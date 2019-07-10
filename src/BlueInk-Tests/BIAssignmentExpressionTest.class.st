Class {
	#name : #BIAssignmentExpressionTest,
	#superclass : #BISingleExpressionTest,
	#category : #'BlueInk-Tests-Base'
}

{ #category : #configurations }
BIAssignmentExpressionTest >> basicConfiguration [

	^ self contextClass basicNew
		formatCommentWithStatements: false
]

{ #category : #test }
BIAssignmentExpressionTest >> testAssignment [
	| source |
	configurationSelector := #basicConfiguration.
	source := self formatExpression: 'a:=1'.
	self assert: source equals: 'a := 1'
]

{ #category : #test }
BIAssignmentExpressionTest >> testAssignmentFormatValue [
	| source |
	configurationSelector := #basicConfiguration.
	source := self formatExpression: 'a:= b:=1'.
	self assert: source equals: 'a := b := 1'
]

{ #category : #test }
BIAssignmentExpressionTest >> testAssignmentFormatVariable [
	| source |
	configurationSelector := #basicConfiguration.
	source := self formatExpression: ' a   := 1'.
	self assert: source equals: 'a := 1'
]

{ #category : #test }
BIAssignmentExpressionTest >> testAssignmentHasNoExtraSpace [
	| source |
	configurationSelector := #basicConfiguration.
	source := self formatExpression: '   a:=1   '.
	self assert: source equals: 'a := 1'
]

{ #category : #test }
BIAssignmentExpressionTest >> testAssignmentHasNoExtraSpaceInside [
	| source |
	configurationSelector := #basicConfiguration.
	source := self formatExpression: '   a    :=    1   '.
	self assert: source equals: 'a := 1'
]

{ #category : #test }
BIAssignmentExpressionTest >> testMultipleAssignmentHasNoExtraSpaceInside [
	| source |
	configurationSelector := #basicConfiguration.
	source := self formatExpression: '   a    :=    b :=  1   '.
	self assert: source equals: 'a := b := 1'
]

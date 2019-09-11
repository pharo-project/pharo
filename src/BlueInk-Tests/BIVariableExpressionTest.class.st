Class {
	#name : #BIVariableExpressionTest,
	#superclass : #BISingleExpressionTest,
	#category : #'BlueInk-Tests-Base'
}

{ #category : #configuration }
BIVariableExpressionTest >> basicConfiguration [

	^ self contextClass basicNew
		formatCommentWithStatements: false
]

{ #category : #tests }
BIVariableExpressionTest >> testNoExtraSpaces [
	| source |
	configurationSelector := #basicConfiguration.
	source := self formatExpression: '    a     '.
	self assert: source equals: 'a'
]

{ #category : #tests }
BIVariableExpressionTest >> testSpaces [
	| source |
	configurationSelector := #basicConfiguration.
	source := self formatExpression: 'a'.
	self assert: source equals: 'a'
]

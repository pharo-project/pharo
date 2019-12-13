Class {
	#name : #BILiteralArrayExpressionTest,
	#superclass : #BISingleExpressionTest,
	#category : #'BlueInk-Tests-Base'
}

{ #category : #configurations }
BILiteralArrayExpressionTest >> basicConfiguration [

	^ self contextClass basicNew
		formatCommentWithStatements: false
]

{ #category : #tests }
BILiteralArrayExpressionTest >> testByteArrayParenthesis [
	| source |
	configurationSelector := #basicConfiguration.
	source := self formatExpression: '#[1 2 3]'.
	self assert: source equals: '#[1 2 3]'
]

{ #category : #tests }
BILiteralArrayExpressionTest >> testNoExtraSpaceArroundParenthesis [
	| source |
	configurationSelector := #basicConfiguration.
	source := self formatExpression: '#(  1 2 3    )  '.
	self assert: source equals: '#(1 2 3)'
]

{ #category : #tests }
BILiteralArrayExpressionTest >> testNoExtraSpaceArroundParenthesis2 [
	| source |
	configurationSelector := #basicConfiguration.
	source := self formatExpression: '#[  1 2 3    ]  '.
	self assert: source equals: '#[1 2 3]'
]

{ #category : #tests }
BILiteralArrayExpressionTest >> testNoExtraSpaceBetweenValue [
	| source |
	configurationSelector := #basicConfiguration.
	source := self formatExpression: '#(1     2       3)'.
	self assert: source equals: '#(1 2 3)'
]

{ #category : #tests }
BILiteralArrayExpressionTest >> testNoExtraSpaceBetweenValue2 [
	| source |
	configurationSelector := #basicConfiguration.
	source := self formatExpression: '#[1     2       3]'.
	self assert: source equals: '#[1 2 3]'
]

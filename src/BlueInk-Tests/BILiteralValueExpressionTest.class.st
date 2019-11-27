Class {
	#name : #BILiteralValueExpressionTest,
	#superclass : #BISingleExpressionTest,
	#category : #'BlueInk-Tests-Base'
}

{ #category : #configurations }
BILiteralValueExpressionTest >> basicConfiguration [

	^ self contextClass basicNew
		formatCommentWithStatements: false
]

{ #category : #tests }
BILiteralValueExpressionTest >> testNoExtraSpaces [
	| source |
	configurationSelector := #basicConfiguration.
	source := self formatExpression: '    2     '.
	self assert: source equals: '2'
]

{ #category : #tests }
BILiteralValueExpressionTest >> testNotLitteral [

	| expr source |
	expr := RBLiteralValueNode value: Transcript. 
	configurationSelector := #basicConfiguration.
	source := self formatter format: expr.
	self assert: source equals: '''<an unprintable nonliteral value>'''
	
]

{ #category : #tests }
BILiteralValueExpressionTest >> testSpaces [
	| source |
	configurationSelector := #basicConfiguration.
	source := self formatExpression: '2'.
	self assert: source equals: '2'
]

Class {
	#name : #EFLiteralValueExpressionTest,
	#superclass : #EFTest,
	#category : #'EnlumineurFormatter-Tests'
}

{ #category : #configurations }
EFLiteralValueExpressionTest >> basicConfiguration [

	^ self contextClass basicNew
		formatCommentCloseToStatements: false
		
]

{ #category : #tests }
EFLiteralValueExpressionTest >> testNoExtraSpaces [
	| source |
	configurationSelector := #basicConfiguration.
	source := self formatExpression: '    2     '.
	self assert: source equals: '2'
]

{ #category : #tests }
EFLiteralValueExpressionTest >> testNotLitteral [

	| expr source |
	expr := RBLiteralValueNode value: Transcript. 
	configurationSelector := #basicConfiguration.
	source := self formatter format: expr.
	self assert: source equals: '''<an unprintable nonliteral value>'''
	
]

{ #category : #tests }
EFLiteralValueExpressionTest >> testNumbers [
	| source |
	configurationSelector := #basicConfiguration.
	source := self formatExpression: ' 3.141592653589793 '.
	self assert: source equals: '3.141592653589793'
	

]

{ #category : #tests }
EFLiteralValueExpressionTest >> testSpaces [
	| source |
	configurationSelector := #basicConfiguration.
	source := self formatExpression: '2'.
	self assert: source equals: '2'
]

{ #category : #tests }
EFLiteralValueExpressionTest >> testString [
	| source |
	configurationSelector := #basicConfiguration.
	source := self formatExpression: ' ''Pharo'' '.
	self assert: source equals: '''Pharo'''
]

{ #category : #tests }
EFLiteralValueExpressionTest >> testTrue [
	| source |
	configurationSelector := #basicConfiguration.
	source := self formatExpression: ' true '.
	self assert: source equals: 'true'
]

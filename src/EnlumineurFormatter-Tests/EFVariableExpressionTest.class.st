Class {
	#name : #EFVariableExpressionTest,
	#superclass : #EFTest,
	#category : #'EnlumineurFormatter-Tests'
}

{ #category : #configuration }
EFVariableExpressionTest >> basicConfiguration [

	^ self contextClass basicNew
		formatCommentCloseToStatements:false
]

{ #category : #tests }
EFVariableExpressionTest >> testNoExtraSpaces [
	| source |
	configurationSelector := #basicConfiguration.
	source := self formatExpression: '    a     '.
	self assert: source equals: 'a'
]

{ #category : #tests }
EFVariableExpressionTest >> testSpaces [
	| source |
	configurationSelector := #basicConfiguration.
	source := self formatExpression: 'a'.
	self assert: source equals: 'a'
]

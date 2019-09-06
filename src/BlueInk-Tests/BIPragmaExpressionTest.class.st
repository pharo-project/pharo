Class {
	#name : #BIPragmaExpressionTest,
	#superclass : #BISingleExpressionTest,
	#category : #'BlueInk-Tests-Base'
}

{ #category : #configuration }
BIPragmaExpressionTest >> basicConfiguration [

	^ self contextClass basicNew
		formatCommentWithStatements:false;
		maxLineLength: 50;
		keepBlockInMessage: true
]

{ #category : #tests }
BIPragmaExpressionTest >> testPragmaNoExtraSpace [
	| expr source |
	expr := self parserClass parsePragma: '<   Pragma   >'.
	configurationSelector := #basicConfiguration.
	source := self formatter format: expr.
	self assert: source equals: '<Pragma>'
]

{ #category : #tests }
BIPragmaExpressionTest >> testPragmaNoExtraSpace2 [
	| expr source |
	expr := self parserClass
		parsePragma: '<func:     ''(3+4)''       res:      7>'.
	configurationSelector := #basicConfiguration.
	source := self formatter format: expr.
	self assert: source equals: '<func: ''(3+4)'' res: 7>'
]

{ #category : #tests }
BIPragmaExpressionTest >> testPragmaNoExtraSpaceBetweenMethodeAndArgument [
	| expr source |
	expr := self parserClass parsePragma: '<primitive:    41>'.
	configurationSelector := #basicConfiguration.
	source := self formatter format: expr.
	self assert: source equals: '<primitive: 41>'
]

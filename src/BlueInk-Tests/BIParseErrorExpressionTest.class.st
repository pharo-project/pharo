Class {
	#name : #BIParseErrorExpressionTest,
	#superclass : #BISingleExpressionTest,
	#category : #'BlueInk-Tests-Base'
}

{ #category : #configuration }
BIParseErrorExpressionTest >> basicConfiguration [

	^ self contextClass basicNew
		formatCommentWithStatements:false;
		retainBlankLinesBetweenStatements: false;
		minimumNewLinesBetweenStatements:0;
		periodsAtEndOfBlock:false;
		periodsAtEndOfMethod:false
]

{ #category : #tests }
BIParseErrorExpressionTest >> testParseError [
	| expr source |
	expr := self parserClass parseFaultyExpression: '#(1 3 4'.
	configurationSelector := #basicConfiguration.
	source := self formatter format: expr.
	self assert: source equals: '(1 3 4'
]

{ #category : #tests }
BIParseErrorExpressionTest >> testParseError2 [
	| expr source |
	expr := self parserClass parseFaultyExpression: '1 := '.
	configurationSelector := #basicConfiguration.
	source := self formatter format: expr.
	self assert: source equals: '1. := '
]

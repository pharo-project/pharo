Class {
	#name : #EFPragmaExpressionTest,
	#superclass : #EFTest,
	#category : #'EnlumineurFormatter-Tests'
}

{ #category : #configuration }
EFPragmaExpressionTest >> basicConfiguration [

	^ self contextClass basicNew
		formatCommentCloseToStatements:false;
		maxLineLength: 50;
		keepBlockInMessage: true
]

{ #category : #tests }
EFPragmaExpressionTest >> testPragmaNoExtraSpace [
	| expr source |
	expr := RBParser parsePragma:'<   Pragma   >'.
	configurationSelector := #basicConfiguration.
	source := self formatter format: expr.
	self assert: source equals: '<Pragma>'
]

{ #category : #tests }
EFPragmaExpressionTest >> testPragmaNoExtraSpace2 [
	| expr source |
	expr := RBParser parsePragma:'<func:     ''(3+4)''       res:      7>'.
	configurationSelector := #basicConfiguration.
	source := self formatter format: expr.
	self assert: source equals: '<func: ''(3+4)'' res: 7>'
]

{ #category : #tests }
EFPragmaExpressionTest >> testPragmaNoExtraSpaceBetweenMethodeAndArgument [
	| expr source |
	expr := RBParser parsePragma:'<primitive:    41>'.
	configurationSelector := #basicConfiguration.
	source := self formatter format: expr.
	self assert: source equals: '<primitive: 41>'
]

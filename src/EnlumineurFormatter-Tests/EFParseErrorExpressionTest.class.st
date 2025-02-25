Class {
	#name : 'EFParseErrorExpressionTest',
	#superclass : 'EFTest',
	#category : 'EnlumineurFormatter-Tests',
	#package : 'EnlumineurFormatter-Tests'
}

{ #category : 'configuration' }
EFParseErrorExpressionTest >> basicConfiguration [

	^ self contextClass basicNew
		formatCommentCloseToStatements:false;
		retainBlankLinesBetweenStatements: false;
		minimumNewLinesBetweenStatements:0;
		periodAtEndOfBlock:false;
		periodAtEndOfMethod:false
]

{ #category : 'tests' }
EFParseErrorExpressionTest >> testParseError [

	| expr source |
	expr := OCParser parseFaultyExpression: '#(1 3 4'.
	configurationSelector := #basicConfiguration.
	source := self newFormatter format: expr.
	self assert: source equals: '#( 1 3 4'
]

{ #category : 'tests' }
EFParseErrorExpressionTest >> testParseError2 [

	| expr source |
	expr := OCParser parseFaultyExpression: '1 := '.
	configurationSelector := #basicConfiguration.
	source := self newFormatter format: expr.
	self assert: source equals: '1. := '
]

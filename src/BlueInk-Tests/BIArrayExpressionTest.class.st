Class {
	#name : #BIArrayExpressionTest,
	#superclass : #BISingleExpressionTest,
	#category : #'BlueInk-Tests-Base'
}

{ #category : #configurations }
BIArrayExpressionTest >> basicConfiguration [

	^ self contextClass basicNew
		formatCommentWithStatements:false;
		maxLineLength:50;
		stringInsideParentheses:'';
		retainBlankLinesBetweenStatements:false;
		minimumNewLinesBetweenStatements:1;
		indentString:'	';
		periodsAtEndOfBlock:false;
		periodsAtEndOfMethod:false
]

{ #category : #configurations }
BIArrayExpressionTest >> maxLineLengthConfiguration [
	"Here we can control explicitly the configuration we want."

	" String streamContents: [:s |
			BIPrettyPrinterContext new storeOn: s]"

	^ self contextClass basicNew
		formatCommentWithStatements:false;
		maxLineLength:12;
		retainBlankLinesBetweenStatements:false;
		minimumNewLinesBetweenStatements: 1;
		indentString: '	';
		periodsAtEndOfBlock:false;
		periodsAtEndOfMethod:false
]

{ #category : #configurations }
BIArrayExpressionTest >> periodAsTerminatorConfiguration [
	"Here we can control explicitly the configuration we want."

	" String streamContents: [:s |
			BIPrettyPrinterContext new storeOn: s]"

	^ self contextClass basicNew
		formatCommentWithStatements:false;
		maxLineLength:12;
		retainBlankLinesBetweenStatements:false;
		minimumNewLinesBetweenStatements: 1;
		indentString: '	';
		periodsAtEndOfBlock:true;
		periodsAtEndOfMethod:true;
		stringInsideBlocks:'';
		lineUpBlockBrackets:false
]

{ #category : #configurations }
BIArrayExpressionTest >> periodAsTerminatorMessageConfiguration2 [
	"Here we can control explicitly the configuration we want."

	" String streamContents: [:s |
			BIPrettyPrinterContext new storeOn: s]"

	^ self contextClass basicNew
		formatCommentWithStatements:false;
		maxLineLength:12;
		retainBlankLinesBetweenStatements:false;
		minimumNewLinesBetweenStatements: 1;
		indentString: '	';
		periodsAtEndOfBlock:true;
		periodsAtEndOfMethod:true;
		multiLineMessages:Array new;
		oneLineMessages:Array new;
		numberOfArgumentsForMultiLine:2;
		newLineBeforeFirstKeyword:false
]

{ #category : #test }
BIArrayExpressionTest >> testEmptyArray [
	| source |
	configurationSelector := #basicConfiguration.
	source := self formatExpression: '{}'.
	self assert: source equals: '{}'
]

{ #category : #test }
BIArrayExpressionTest >> testFormatElement [
	| source |
	configurationSelector := #basicConfiguration.
	source := self formatExpression: ' { a:=1. b:=2 } '.
	self assert: source equals:
'{(a := 1).
(b := 2)}'
]

{ #category : #test }
BIArrayExpressionTest >> testNoExtraSpaceAroundPeriod [
	| source |
	configurationSelector := #basicConfiguration.
	source := self formatExpression: '{1.    2.        3}'.
	self assert: source equals: '{1 . 2 . 3}'
]

{ #category : #test }
BIArrayExpressionTest >> testNoSpaceAroundBrackets [
	| source |
	configurationSelector := #basicConfiguration.
	source := self formatExpression: ' { 1. 2. 3 } '.
	self assert: source equals: '{1 . 2 . 3}'
]

{ #category : #test }
BIArrayExpressionTest >> testPeriodAtEndOfBlock [
	| source |
	configurationSelector := #periodAsTerminatorConfiguration.
	source := self formatExpression: '{[1]}'.
	self assert: source equals: '{[1.].}'
]

{ #category : #test }
BIArrayExpressionTest >> testPeriodAtEndOfMethod [
	| source |
	configurationSelector := #periodAsTerminatorMessageConfiguration2.
	source := self formatExpression: '{1 factorial}'.
	self assert: source equals: '{1 factorial.}'
]

{ #category : #test }
BIArrayExpressionTest >> testSpaceAroundPeriod [
	| source |
	configurationSelector := #basicConfiguration.
	source := self formatExpression: '{1. 2. 3}'.
	self assert: source equals: '{1 . 2 . 3}'
]

{ #category : #test }
BIArrayExpressionTest >> testToLongNewLine [
	| source |
	configurationSelector := #maxLineLengthConfiguration.
	source := self formatExpression: '{VeryLongExpression. 1}'.
	self
		assert: source
		equals:
			'{VeryLongExpression.
1}'
]

{ #category : #test }
BIArrayExpressionTest >> testToLongNewLine2 [
	| source |
	configurationSelector := #maxLineLengthConfiguration.
	source := self formatExpression: '{1. 2. 3. 4. 5. 6. 7. 1}'.
	self
		assert: source
		equals:
			'{1 . 2 . 3 . 4.
5 . 6 . 7 . 1}'
]

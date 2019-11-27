Class {
	#name : #BISequenceExpressionTest,
	#superclass : #BISingleExpressionTest,
	#category : #'BlueInk-Tests-Base'
}

{ #category : #configurations }
BISequenceExpressionTest >> basicConfiguration [ 

	^ self contextClass basicNew
		formatCommentWithStatements:true;
		indentString: '	';
		newLinesAfterTemporaries:0;
		retainBlankLinesBeforeComments:false;
		useBasicCommentFormat:false;
		maxLineLength:50;
		retainBlankLinesBetweenStatements:false;
		minimumNewLinesBetweenStatements:0;
		periodsAtEndOfBlock:false;
		periodsAtEndOfMethod:false
]

{ #category : #configurations }
BISequenceExpressionTest >> dontRetainBlankLinesBeforeCommentsConfiguration [
	"Here we can control explicitly the configuration we want."

	" String streamContents: [:s |
			BIPrettyPrinterContext new storeOn: s]"


		
	^ self contextClass basicNew
		retainBlankLinesBeforeComments:false;
		formatCommentWithStatements:true;
		indentString: '	';
		newLinesAfterTemporaries:0;
		useBasicCommentFormat:false;
		maxLineLength:50;
		retainBlankLinesBetweenStatements:false;
		minimumNewLinesBetweenStatements:0;
		periodsAtEndOfBlock:false;
		periodsAtEndOfMethod:false
]

{ #category : #configurations }
BISequenceExpressionTest >> dontRetainBlankLinesBetweenStatementsConfiguration [
	"Here we can control explicitly the configuration we want."

	" String streamContents: [:s |
			BIPrettyPrinterContext new storeOn: s]"

	^ self contextClass basicNew
		retainBlankLinesBetweenStatements: false;
		retainBlankLinesBeforeComments:false;
		formatCommentWithStatements:true;
		indentString: '	';
		newLinesAfterTemporaries:0;
		useBasicCommentFormat:false;
		maxLineLength:50;
		minimumNewLinesBetweenStatements:0;
		periodsAtEndOfBlock:false;
		periodsAtEndOfMethod:false
]

{ #category : #configurations }
BISequenceExpressionTest >> newLineAfterStatementConfiguration [

	^ self contextClass basicNew
		minimumNewLinesBetweenStatements:1;
		formatCommentWithStatements:true;
		indentString: '	';
		newLinesAfterTemporaries:0;
		retainBlankLinesBeforeComments:false;
		useBasicCommentFormat:false;
		maxLineLength:50;
		retainBlankLinesBetweenStatements:false;
		periodsAtEndOfBlock:false;
		periodsAtEndOfMethod:false
]

{ #category : #configurations }
BISequenceExpressionTest >> newLineAfterTemporariesConfiguration [

	^ self contextClass basicNew
		formatCommentWithStatements:true;
		indentString: '	';
		newLinesAfterTemporaries:1
]

{ #category : #configurations }
BISequenceExpressionTest >> periodAsTerminatorConfiguration [
	"Here we can control explicitly the configuration we want."

	" String streamContents: [:s |
			BIPrettyPrinterContext new storeOn: s]"

	^ self contextClass basicNew
		periodsAtEndOfBlock:true;
		periodsAtEndOfMethod:true;
		formatCommentWithStatements:true;
		indentString: '	';
		newLinesAfterTemporaries:0;
		retainBlankLinesBeforeComments:false;
		useBasicCommentFormat:false;
		maxLineLength:50;
		retainBlankLinesBetweenStatements:false;
		minimumNewLinesBetweenStatements:0;
		stringInsideBlocks: ' ';
		lineUpBlockBrackets: true;
		multiLineMessages:Array new;
		oneLineMessages:Array new;
		numberOfArgumentsForMultiLine:2;
		newLineBeforeFirstKeyword:false
]

{ #category : #configurations }
BISequenceExpressionTest >> periodNotAsTerminatorConfiguration [ 

	^ self contextClass basicNew
		periodsAtEndOfBlock:false;
		periodsAtEndOfMethod:false;
		formatCommentWithStatements:true;
		indentString: '	';
		newLinesAfterTemporaries:0;
		retainBlankLinesBeforeComments:false;
		useBasicCommentFormat:false;
		maxLineLength:50;
		retainBlankLinesBetweenStatements:false;
		minimumNewLinesBetweenStatements:0;
		stringInsideBlocks: ' ';
		lineUpBlockBrackets: true;
		multiLineMessages:Array new;
		oneLineMessages:Array new;
		numberOfArgumentsForMultiLine:2;
		newLineBeforeFirstKeyword:false
]

{ #category : #configurations }
BISequenceExpressionTest >> retainBlankLinesBeforeCommentsConfiguration [
	"Here we can control explicitly the configuration we want."

	" String streamContents: [:s |
			BIPrettyPrinterContext new storeOn: s]"

	^ self contextClass basicNew
		retainBlankLinesBeforeComments:true;
		formatCommentWithStatements:true;
		indentString: '	';
		newLinesAfterTemporaries:0;
		useBasicCommentFormat:false;
		maxLineLength:50;
		retainBlankLinesBetweenStatements:false;
		minimumNewLinesBetweenStatements:0;
		periodsAtEndOfBlock:false;
		periodsAtEndOfMethod:false
]

{ #category : #configurations }
BISequenceExpressionTest >> retainBlankLinesBetweenStatementsConfiguration [
	"Here we can control explicitly the configuration we want."

	" String streamContents: [:s |
			BIPrettyPrinterContext new storeOn: s]"

	^ self contextClass basicNew
		retainBlankLinesBetweenStatements: true;
		retainBlankLinesBeforeComments:false;
		formatCommentWithStatements:true;
		indentString: '	';
		newLinesAfterTemporaries:0;
		useBasicCommentFormat:false;
		maxLineLength:50;
		minimumNewLinesBetweenStatements:0;
		periodsAtEndOfBlock:false;
		periodsAtEndOfMethod:false
]

{ #category : #tests }
BISequenceExpressionTest >> testCommentBetweenStatements [
	| source |
	configurationSelector := #basicConfiguration.
	source := self formatExpression: '|a b| a := 1."comment"b := 2'.
	self assert: source equals: '| a b |a := 1.	"comment" b := 2'
]

{ #category : #tests }
BISequenceExpressionTest >> testCommentBetweenTemporariesAndStatement [
	| source |
	configurationSelector := #basicConfiguration.
	source := self formatExpression: '|a b| "comment" a := 1'.
	self
		assert: source
		equals:
			'| a b |"comment"
a := 1'
]

{ #category : #tests }
BISequenceExpressionTest >> testDontRetainBlankLinesBeforeComments [
	| source |
	configurationSelector := #dontRetainBlankLinesBeforeCommentsConfiguration.
	source := self
		formatExpression:
			'|a b|
	a :=1.
	"Comment" b :=2'.
	self assert: source equals: '| a b |a := 1.	"Comment" b := 2'
]

{ #category : #tests }
BISequenceExpressionTest >> testDontRetainBlankLinesBetweenStatements [
	| source |
	configurationSelector := #dontRetainBlankLinesBetweenStatementsConfiguration.
	source := self
		formatExpression:
			'|a b| 
	
	a:=1.
	
	b:=a'.
	self assert: source equals: '| a b |a := 1. b := a'
]

{ #category : #tests }
BISequenceExpressionTest >> testFormatEachStatement [
	| source |
	configurationSelector := #basicConfiguration.
	source := self formatExpression: '|a b|a:=1.b:=a'.
	self assert: source equals: '| a b |a := 1. b := a'
]

{ #category : #tests }
BISequenceExpressionTest >> testLineBetweenTemporariesAndComment [
	| source |
	configurationSelector := #newLineAfterTemporariesConfiguration.
	source := self formatExpression: '|a b| "comment"'.
	self
		assert: source
		equals:
			'| a b |
"comment"
'
]

{ #category : #tests }
BISequenceExpressionTest >> testNewLineBetweenStatement [
	| source |
	configurationSelector := #newLineAfterStatementConfiguration.
	source := self formatExpression: '|a b| a := 1. b := a'.
	self
		assert: source
		equals:
			'| a b |a := 1.
b := a'
]

{ #category : #tests }
BISequenceExpressionTest >> testNoPeriodAtEndOfBlock [
	| source |
	configurationSelector := #periodNotAsTerminatorConfiguration.
	source := self formatExpression: '|a b| [1.].'.
	self assert: source equals: '| a b |[ 1 ]'
]

{ #category : #tests }
BISequenceExpressionTest >> testNoPeriodAtEndOfMethode [
	| source |
	configurationSelector := #periodNotAsTerminatorConfiguration.
	source := self formatExpression: '|a b| 6 factorial'.
	self assert: source equals: '| a b |6 factorial'
]

{ #category : #tests }
BISequenceExpressionTest >> testPeriodAtEndOfBlock [
	| source |
	configurationSelector := #periodAsTerminatorConfiguration.
	source := self formatExpression: '|a b| [1.].'.
	self assert: source equals: '| a b |[ 1. ].'
]

{ #category : #tests }
BISequenceExpressionTest >> testPeriodAtEndOfMethode [
	| source |
	configurationSelector := #periodAsTerminatorConfiguration.
	source := self formatExpression: '|a b| 6 factorial'.
	self assert: source equals: '| a b |6 factorial.'
]

{ #category : #tests }
BISequenceExpressionTest >> testRetainBlankLinesBeforeComments [
	| source |
	configurationSelector := #retainBlankLinesBeforeCommentsConfiguration.
	source := self
		formatExpression:
			'|a b|
	a :=1.
	"Comment"
	b :=2'.
	self
		assert: source
		equals:
			'| a b |a := 1.
"Comment" b := 2'
]

{ #category : #tests }
BISequenceExpressionTest >> testRetainBlankLinesBetweenStatements [
	| source |
	configurationSelector := #retainBlankLinesBetweenStatementsConfiguration.
	source := self
		formatExpression:
			'|a b| 
	
	a:=1.
	
	b:=a'.
	self
		assert: source
		equals:
			'| a b |a := 1.

b := a'
]

{ #category : #tests }
BISequenceExpressionTest >> testRetainBlankLinesBetweenStatements2 [
	| source |
	configurationSelector := #retainBlankLinesBetweenStatementsConfiguration.
	source := self
		formatExpression:
			'|a b| 
	
	a:=1.
	
	
	b:=a'.
	self
		assert: source
		equals:
			'| a b |a := 1.


b := a'
]

{ #category : #tests }
BISequenceExpressionTest >> testRetainBlankLinesBetweenStatements3 [
	| source |
	configurationSelector := #retainBlankLinesBetweenStatementsConfiguration.
	source := self
		formatExpression:
			'|a b| 
	
	a:=1.
	b:=a'.
	self
		assert: source
		equals:
			'| a b |a := 1.
b := a'
]

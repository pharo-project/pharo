Class {
	#name : #EFSequenceExpressionTest,
	#superclass : #EFTest,
	#category : #'EnlumineurFormatter-Tests'
}

{ #category : #configurations }
EFSequenceExpressionTest >> basicConfiguration [ 

	^ self contextClass basicNew
		formatCommentCloseToStatements:true;
		indentStyle: #tabulation;
		numberOfNewLinesAfterTemporaries:0;
		retainBlankLinesBeforeComments:false;
		useBasicCommentFormat:false;
		maxLineLength:50;
		retainBlankLinesBetweenStatements:false;
		minimumNewLinesBetweenStatements:0;
		periodAtEndOfBlock:false;
		periodAtEndOfMethod:false;
		indentExtraSpaces:0
]

{ #category : #configurations }
EFSequenceExpressionTest >> dontFormatCommentWithStatementConfiguration [

	^ self contextClass basicNew
		formatCommentCloseToStatements:false;
		indentStyle: #tabulation;
		numberOfNewLinesAfterTemporaries:0;
		retainBlankLinesBeforeComments:false;
		useBasicCommentFormat:false;
		maxLineLength:50;
		retainBlankLinesBetweenStatements:false;
		minimumNewLinesBetweenStatements:0;
		periodAtEndOfBlock:false;
		periodAtEndOfMethod:false;
		multiLineMessages: Array new;
		oneLineMessages: Array new;
		numberOfArgumentsForMultiLine: 5;
		indentsForKeywords: 0;
		keepBlockInMessage: false;
		newLineBeforeFirstKeyword:false;
		indentExtraSpaces: 0
]

{ #category : #configurations }
EFSequenceExpressionTest >> dontRetainBlankLinesBeforeCommentsConfiguration [
	"Here we can control explicitely the configuration we want."

	" String streamContents: [:s |
			BIPrettyPrinterContext new storeOn: s]"


		
	^ self contextClass basicNew
		retainBlankLinesBeforeComments:false;
		formatCommentCloseToStatements:true;
		indentStyle: #tabulation;
		numberOfNewLinesAfterTemporaries:0;
		useBasicCommentFormat:false;
		maxLineLength:50;
		retainBlankLinesBetweenStatements:false;
		minimumNewLinesBetweenStatements:0;
		periodAtEndOfBlock:false;
		periodAtEndOfMethod:false;
		indentExtraSpaces:0
]

{ #category : #configurations }
EFSequenceExpressionTest >> dontRetainBlankLinesBetweenStatementsConfiguration [
	"Here we can control explicitely the configuration we want."

	" String streamContents: [:s |
			BIPrettyPrinterContext new storeOn: s]"

	^ self contextClass basicNew
		retainBlankLinesBetweenStatements: false;
		retainBlankLinesBeforeComments:false;
		formatCommentCloseToStatements:true;
		indentStyle: #tabulation;
		numberOfNewLinesAfterTemporaries:0;
		useBasicCommentFormat:false;
		maxLineLength:50;
		minimumNewLinesBetweenStatements:0;
		periodAtEndOfBlock:false;
		periodAtEndOfMethod:false;
		indentExtraSpaces:0
]

{ #category : #configurations }
EFSequenceExpressionTest >> formatBlockConfiguration [

	^ self contextClass basicNew
		formatCommentCloseToStatements:false;
		indentStyle: #tabulation;
		numberOfNewLinesAfterTemporaries:1;
		retainBlankLinesBeforeComments:false;
		useBasicCommentFormat:false;
		maxLineLength:50;
		retainBlankLinesBetweenStatements:false;
		minimumNewLinesBetweenStatements:0;
		periodAtEndOfBlock:false;
		periodAtEndOfMethod:false;
		multiLineMessages: Array new;
		oneLineMessages: Array new;
		numberOfArgumentsForMultiLine: 5;
		indentsForKeywords: 1;
		keepBlockInMessage: true;
		newLineBeforeFirstKeyword:true;
		indentExtraSpaces: 0;
		numberOfSpacesInsideBlock: 1;
		lineUpBlockBrackets: false
]

{ #category : #configurations }
EFSequenceExpressionTest >> formatCommentWithStatementConfiguration [

	^ self contextClass basicNew
		formatCommentCloseToStatements:true;
		indentStyle: #tabulation;
		numberOfNewLinesAfterTemporaries:0;
		retainBlankLinesBeforeComments:false;
		useBasicCommentFormat:false;
		maxLineLength:50;
		retainBlankLinesBetweenStatements:false;
		minimumNewLinesBetweenStatements:0;
		periodAtEndOfBlock:false;
		periodAtEndOfMethod:false;
		multiLineMessages: Array new;
		oneLineMessages: Array new;
		numberOfArgumentsForMultiLine: 5;
		indentsForKeywords: 0;
		keepBlockInMessage: false;
		newLineBeforeFirstKeyword: false
]

{ #category : #configurations }
EFSequenceExpressionTest >> newLineAfterStatementConfiguration [

	^ self contextClass basicNew
		minimumNewLinesBetweenStatements:1;
		formatCommentCloseToStatements:true;
		indentStyle: #tabulation;
		numberOfNewLinesAfterTemporaries:0;
		retainBlankLinesBeforeComments:false;
		useBasicCommentFormat:false;
		maxLineLength:50;
		retainBlankLinesBetweenStatements:false;
		periodAtEndOfBlock:false;
		periodAtEndOfMethod:false;
		indentExtraSpaces:0
]

{ #category : #configurations }
EFSequenceExpressionTest >> newLineAfterTemporariesConfiguration [

	^ self contextClass basicNew
		formatCommentCloseToStatements:true;
		indentStyle: #tabulation;
		numberOfNewLinesAfterTemporaries:1;
		indentExtraSpaces:0
]

{ #category : #configurations }
EFSequenceExpressionTest >> noPeriodAtEndOfMethodConfiguration [ 

	^ self contextClass basicNew
		periodAtEndOfBlock:false;
		periodAtEndOfMethod:false;
		formatCommentCloseToStatements:true;
		indentStyle: #tabulation;
		numberOfNewLinesAfterTemporaries:0;
		retainBlankLinesBeforeComments:false;
		useBasicCommentFormat:false;
		maxLineLength:50;
		retainBlankLinesBetweenStatements:false;
		minimumNewLinesBetweenStatements:0;
		numberOfSpacesInsideBlock:1;
		lineUpBlockBrackets: false;
		multiLineMessages:Array new;
		oneLineMessages:Array new;
		numberOfArgumentsForMultiLine:2;
		newLineBeforeFirstKeyword:false;
		indentExtraSpaces:0
]

{ #category : #configurations }
EFSequenceExpressionTest >> periodAtEndOfMethodConfiguration [
	"Here we can control explicitely the configuration we want."

	" String streamContents: [:s |
			BIPrettyPrinterContext new storeOn: s]"

	^ self contextClass basicNew
		periodAtEndOfBlock:true;
		periodAtEndOfMethod:true;
		formatCommentCloseToStatements:true;
		indentStyle: #tabulation;
		numberOfNewLinesAfterTemporaries:0;
		retainBlankLinesBeforeComments:false;
		useBasicCommentFormat:false;
		maxLineLength:50;
		retainBlankLinesBetweenStatements:false;
		minimumNewLinesBetweenStatements:0;
		numberOfSpacesInsideBlock:1;
		lineUpBlockBrackets: true;
		multiLineMessages:Array new;
		oneLineMessages:Array new;
		numberOfArgumentsForMultiLine:2;
		newLineBeforeFirstKeyword:false;
		indentExtraSpaces:0
]

{ #category : #configurations }
EFSequenceExpressionTest >> retainBlankLinesBeforeCommentsConfiguration [
	"Here we can control explicitely the configuration we want."

	" String streamContents: [:s |
			BIPrettyPrinterContext new storeOn: s]"

	^ self contextClass basicNew
		retainBlankLinesBeforeComments:true;
		formatCommentCloseToStatements:true;
		indentStyle: #tabulation;
		numberOfNewLinesAfterTemporaries:0;
		useBasicCommentFormat:false;
		maxLineLength:50;
		retainBlankLinesBetweenStatements:false;
		minimumNewLinesBetweenStatements:0;
		periodAtEndOfBlock:false;
		periodAtEndOfMethod:false;
		indentExtraSpaces:0
]

{ #category : #configurations }
EFSequenceExpressionTest >> retainBlankLinesBetweenStatementsConfiguration [
	"Here we can control explicitely the configuration we want."

	" String streamContents: [:s |
			BIPrettyPrinterContext new storeOn: s]"

	^ self contextClass basicNew
		retainBlankLinesBetweenStatements: true;
		retainBlankLinesBeforeComments:false;
		formatCommentCloseToStatements:true;
		indentStyle: #tabulation;
		numberOfNewLinesAfterTemporaries:0;
		useBasicCommentFormat:false;
		maxLineLength:50;
		minimumNewLinesBetweenStatements:0;
		periodAtEndOfBlock:false;
		periodAtEndOfMethod:false;
		indentExtraSpaces:0
]

{ #category : #tests }
EFSequenceExpressionTest >> testCommentBetweenStatements [
	| source |
	configurationSelector := #basicConfiguration.
	source := self formatExpression: '|a b| a := 1."comment"b := 2'.
	self assert: source equals: '| a b |a := 1. "comment" b := 2'
]

{ #category : #tests }
EFSequenceExpressionTest >> testCommentBetweenTemporariesAndStatement [
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
EFSequenceExpressionTest >> testDontFormatCommentWithStatement [
	| source |
	configurationSelector := #dontFormatCommentWithStatementConfiguration.
	source := self formatExpression: '1+3 "We do an addition". 2'.
	self assert: source equals:
 '1 + 3 "We do an addition". 2'
]

{ #category : #tests }
EFSequenceExpressionTest >> testDontRetainBlankLinesBeforeComments [
	| source |
	configurationSelector := #dontRetainBlankLinesBeforeCommentsConfiguration.
	source := self
		formatExpression:
			'|a b|
	a :=1.
	"Comment" b :=2'.
	self assert: source equals: '| a b |a := 1. "Comment" b := 2'
]

{ #category : #tests }
EFSequenceExpressionTest >> testDontRetainBlankLinesBetweenStatements [
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
EFSequenceExpressionTest >> testFormatCommentWithStatement [
	| source |
	configurationSelector := #formatCommentWithStatementConfiguration.
	source := self formatExpression: '1+1 "We do an addition". 2'.
	self assert: source equals: 
'1 + 1. "We do an addition" 2'
]

{ #category : #tests }
EFSequenceExpressionTest >> testFormatEachStatement [
	| source |
	configurationSelector := #basicConfiguration.
	source := self formatExpression: '|a b|a:=1.b:=a'.
	self assert: source equals: '| a b |a := 1. b := a'
]

{ #category : #tests }
EFSequenceExpressionTest >> testLineBetweenTemporariesAndComment [
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
EFSequenceExpressionTest >> testNewLineBetweenStatement [
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
EFSequenceExpressionTest >> testNoNewLineBetweenVariableAndIfTrue [
	| source |
	configurationSelector := #formatBlockConfiguration.
	source := self formatExpression: 
'| x aBitBlt foreColorVal foreColorAlpha gammaTable gammaInverseTable |
	"x ifTrue: [ 
		aBitBlt
			copyBitsColor: foreColorVal
			alpha: foreColorAlpha
			gammaTable: gammaTable
			ungammaTable: gammaInverseTable ]		
"
	x ifTrue: [ 
			aBitBlt
				copyBitsColor: foreColorVal
				alpha: foreColorAlpha
				gammaTable: gammaTable
				ungammaTable: gammaInverseTable ]'.
	self assert: source equals: 
'| x aBitBlt foreColorVal foreColorAlpha gammaTable gammaInverseTable |
"x ifTrue: [ 
		aBitBlt
			copyBitsColor: foreColorVal
			alpha: foreColorAlpha
			gammaTable: gammaTable
			ungammaTable: gammaInverseTable ]		
"
x ifTrue: [ 
	aBitBlt
		copyBitsColor: foreColorVal
		alpha: foreColorAlpha
		gammaTable: gammaTable
		ungammaTable: gammaInverseTable ]'
]

{ #category : #tests }
EFSequenceExpressionTest >> testNoPeriodAtEndOfMethode [
	| source |
	configurationSelector := #noPeriodAtEndOfMethodConfiguration.
	source := self formatExpression: '|a b| 6 factorial'.
	self assert: source equals: '| a b |6 factorial'
]

{ #category : #tests }
EFSequenceExpressionTest >> testPeriodAtEndOfMethode [
	| source |
	configurationSelector := #periodAtEndOfMethodConfiguration.
	source := self formatExpression: '|a b| 6 factorial'.
	self assert: source equals: '| a b |6 factorial.'
]

{ #category : #tests }
EFSequenceExpressionTest >> testRetainBlankLinesBeforeComments [
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
EFSequenceExpressionTest >> testRetainBlankLinesBetweenStatements [
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
EFSequenceExpressionTest >> testRetainBlankLinesBetweenStatements2 [
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
EFSequenceExpressionTest >> testRetainBlankLinesBetweenStatements3 [
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

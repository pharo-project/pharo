Class {
	#name : #BIBlock2ExpressionTest,
	#superclass : #BISingleExpressionTest,
	#category : #'BlueInk-Tests-Base'
}

{ #category : #configuration }
BIBlock2ExpressionTest >> basicConfiguration [

	^ self contextClass basicNew
		stringInsideBlocks:' ';
		periodsAtEndOfBlock:false;
		periodsAtEndOfMethod: false;
		lineUpBlockBrackets:false;
		indentString: '	';
		keepBlockInMessage:false;
		retainBlankLinesBetweenStatements:false;
		retainBlankLinesBeforeComments:false;
		minimumNewLinesBetweenStatements:1;
		multiLineMessages: Array new;
		oneLineMessages: Array new;
		numberOfArgumentsForMultiLine:5;
		indentsForKeywords:1;
		newLineBeforeFirstKeyword:false;
		formatCommentWithStatements:true;
		maxLineLength:50;
		useBasicCommentFormat:false
]

{ #category : #test }
BIBlock2ExpressionTest >> testSpaceAfterComment [
	| source |
	configurationSelector := #basicConfiguration.
	source := self formatExpression: '["aComment"1]'.
	self assert: source equals: '[ "aComment" 1 ]'
]

{ #category : #test }
BIBlock2ExpressionTest >> testformatComment [
	| source |
	configurationSelector := #basicConfiguration.
	source := self formatExpression: '["  c1   " "   c2 " 2 "c3"]'.
	self assert: source equals: '[ "  c1   " "   c2 " 2	"c3" ]'
]

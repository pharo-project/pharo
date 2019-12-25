Class {
	#name : #BIBlockExpressionTest,
	#superclass : #BISingleExpressionTest,
	#category : #'BlueInk-Tests-Base'
}

{ #category : #configurations }
BIBlockExpressionTest >> basicConfiguration [

	^ self contextClass basicNew
		stringInsideBlocks:' ';
		periodsAtEndOfBlock:false;
		periodsAtEndOfMethod: false;
		lineUpBlockBrackets:false;
		indentString: '	';
		keepBlockInMessage:false;
		retainBlankLinesBetweenStatements:false;
		minimumNewLinesBetweenStatements:1;
		multiLineMessages: Array new;
		oneLineMessages: Array new;
		numberOfArgumentsForMultiLine:5;
		indentsForKeywords:1;
		newLineBeforeFirstKeyword:false;
		formatCommentWithStatements:true;
		maxLineLength:50
]

{ #category : #hooks }
BIBlockExpressionTest >> closeBracket [
	^']'
]

{ #category : #configurations }
BIBlockExpressionTest >> newLineBeforeEndBracketConfiguration [
	"Here we can control explicitly the configuration we want."

	" String streamContents: [:s |
			BIPrettyPrinterContext new storeOn: s]"

	^ self contextClass basicNew
		stringInsideBlocks: ' ';
		periodsAtEndOfBlock: false;
		periodsAtEndOfMethod: false;
		lineUpBlockBrackets: true;
		indentString: '	';
		keepBlockInMessage: false;
		retainBlankLinesBetweenStatements: false;
		minimumNewLinesBetweenStatements: 1;
		multiLineMessages: Array new;
		oneLineMessages: Array new;
		numberOfArgumentsForMultiLine: 5;
		indentsForKeywords: 1;
		newLineBeforeFirstKeyword: false;
		formatCommentWithStatements: true;
		maxLineLength: 50
]

{ #category : #configurations }
BIBlockExpressionTest >> noNewLineBeforeEndBracketConfiguration [
	"Here we can control explicitly the configuration we want."

	" String streamContents: [:s |
			BIPrettyPrinterContext new storeOn: s]"

	^ self contextClass basicNew
		stringInsideBlocks: ' ';
		periodsAtEndOfBlock: false;
		periodsAtEndOfMethod: false;
		lineUpBlockBrackets: false;
		indentString: '	';
		keepBlockInMessage: false;
		retainBlankLinesBetweenStatements: false;
		minimumNewLinesBetweenStatements: 1;
		multiLineMessages: Array new;
		oneLineMessages: Array new;
		numberOfArgumentsForMultiLine: 5;
		indentsForKeywords: 1;
		newLineBeforeFirstKeyword: false;
		formatCommentWithStatements: true;
		maxLineLength: 50
]

{ #category : #hooks }
BIBlockExpressionTest >> openBracket [
	^ '['
]

{ #category : #hooks }
BIBlockExpressionTest >> parser [
	^ RBParser
]

{ #category : #configurations }
BIBlockExpressionTest >> periodAtEndConfiguration [
	"Here we can control explicitly the configuration we want."

	" String streamContents: [:s |
			BIPrettyPrinterContext new storeOn: s]"

	^ self contextClass basicNew
		stringInsideBlocks: ' ';
		periodsAtEndOfBlock: true;
		periodsAtEndOfMethod: false;
		lineUpBlockBrackets: false;
		indentString: '	';
		keepBlockInMessage: false;
		retainBlankLinesBetweenStatements: false;
		minimumNewLinesBetweenStatements: 1;
		multiLineMessages: Array new;
		oneLineMessages: Array new;
		numberOfArgumentsForMultiLine: 5;
		indentsForKeywords: 1;
		newLineBeforeFirstKeyword: false;
		formatCommentWithStatements: true;
		maxLineLength: 50
]

{ #category : #test }
BIBlockExpressionTest >> testArgumentIsFormated [
	self validate: ':i|i' isFormattedAs: ' :i | i ' 
]

{ #category : #test }
BIBlockExpressionTest >> testArgumentsAreFormated [
	
	self 
		validate:':i :j| i '
		isFormattedAs:' :i :j | i '.
]

{ #category : #test }
BIBlockExpressionTest >> testArgumentsComments [
	
	self 
		validate:':i "aComment" :j "anotherComment"| i '
		isFormattedAs:
' :i"aComment"
 :j"anotherComment"
 | i '
]

{ #category : #test }
BIBlockExpressionTest >> testEndPeriodInside [
	
	self 
		validate:'2.'
		isFormattedAs:' 2. '
		with: #periodAtEndConfiguration
]

{ #category : #test }
BIBlockExpressionTest >> testFormatBody [

	self 
		validate:' [1]. [ a:=1] '
		isFormattedAs:
' [ 1 ].
[ a := 1 ] '.
]

{ #category : #test }
BIBlockExpressionTest >> testMultiline [

	self 
		validate: '1+3. 6 factorial '
		isFormattedAs: 
' 1 + 3.
6 factorial '
]

{ #category : #test }
BIBlockExpressionTest >> testNewLineAfterComment [
	
	self 
		validate: '"aComment"1. 2'
		isFormattedAs: 
' "aComment"
1.
2 '
]

{ #category : #test }
BIBlockExpressionTest >> testNewLineBeforeEndBrackets [

	self 
		validate:'3. 4'  
		isFormattedAs:
' 3.
4
'  		with: #newLineBeforeEndBracketConfiguration
]

{ #category : #test }
BIBlockExpressionTest >> testNoEndPeriodInside [

	self validate: '2.' isFormattedAs: ' 2 '
]

{ #category : #test }
BIBlockExpressionTest >> testNoExtraSpaceAroundBlock [

	| expr source |
	expr := self parser parseExpression: '   ', self openBracket, '2', self closeBracket, '   '.
	configurationSelector := #basicConfiguration.
	source := self formatter format: expr.
	self assert: source equals: self openBracket, ' 2 ', self closeBracket
]

{ #category : #test }
BIBlockExpressionTest >> testNoExtraSpaceInside [

	self validate: '    2     ' isFormattedAs: ' 2 '
]

{ #category : #test }
BIBlockExpressionTest >> testNoNewLineBeforeEndBrackets [

	self 
		validate: '3. 4'  
		isFormattedAs:
' 3.
4 '  with: #noNewLineBeforeEndBracketConfiguration
]

{ #category : #test }
BIBlockExpressionTest >> testSpace [

	self validate: '2' isFormattedAs: ' 2 '
]

{ #category : #test }
BIBlockExpressionTest >> validate: stringInsideBlock isFormattedAs: expectedStringInsideBlock [
	self validate: stringInsideBlock isFormattedAs: expectedStringInsideBlock with: #basicConfiguration
]

{ #category : #test }
BIBlockExpressionTest >> validate: stringInsideBlock isFormattedAs: expectedStringInsideBlock with: aConfiguration [
	| expr source |
	expr := self parser parseExpression: self openBracket, stringInsideBlock, self closeBracket.
	configurationSelector := aConfiguration.
	source := self formatter format: expr.
	self assert: source equals: self openBracket, expectedStringInsideBlock, self closeBracket
]

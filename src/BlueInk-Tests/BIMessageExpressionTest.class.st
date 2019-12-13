Class {
	#name : #BIMessageExpressionTest,
	#superclass : #BISingleExpressionTest,
	#category : #'BlueInk-Tests-Base'
}

{ #category : #configurations }
BIMessageExpressionTest >> basicConfiguration [

	^ self contextClass basicNew
		indentString: '	';
		formatCommentWithStatements:false;
		multiLineMessages:Array new;
		oneLineMessages:Array new;
		numberOfArgumentsForMultiLine:10;
		indentsForKeywords: 0;
		maxLineLength: 50;
		newLineBeforeFirstKeyword:false;
		keepBlockInMessage:false
]

{ #category : #configurations }
BIMessageExpressionTest >> keepBlockInMessageConfiguration [

	^ self contextClass basicNew
		indentString: '	';
		formatCommentWithStatements:false;
		multiLineMessages:Array new;
		oneLineMessages:Array new;
		numberOfArgumentsForMultiLine:10;
		indentsForKeywords: 0;
		maxLineLength: 20;
		newLineBeforeFirstKeyword:false;
		keepBlockInMessage: true;
		stringInsideBlocks: '';
		stringFollowingReturn: '';
		periodsAtEndOfBlock: false;
		periodsAtEndOfMethod: false;
		lineUpBlockBrackets: false;
		retainBlankLinesBetweenStatements: false;
		minimumNewLinesBetweenStatements: 0
]

{ #category : #configurations }
BIMessageExpressionTest >> multiLineMessageConfiguration [

	^ self contextClass basicNew
		indentString: '	';
		formatCommentWithStatements:false;
		multiLineMessages:#(#multi:line:);
		oneLineMessages:Array new;
		numberOfArgumentsForMultiLine:10;
		indentsForKeywords: 1;
		maxLineLength: 50;
		newLineBeforeFirstKeyword:false;
		keepBlockInMessage:false
]

{ #category : #configurations }
BIMessageExpressionTest >> newLineBeforeFirstKeywordConfiguration [

	^ self contextClass basicNew
		indentString: '	';
		formatCommentWithStatements:false;
		multiLineMessages:Array new;
		oneLineMessages:Array new;
		numberOfArgumentsForMultiLine:10;
		indentsForKeywords: 0;
		maxLineLength: 50;
		newLineBeforeFirstKeyword:true;
		keepBlockInMessage:false
]

{ #category : #configurations }
BIMessageExpressionTest >> noIdentsForKeyWordConfiguration [
	"Here we can control explicitly the configuration we want."

	" String streamContents: [:s |
			BIPrettyPrinterContext new storeOn: s]"
	
	^ self contextClass basicNew
		indentString: '	';
		formatCommentWithStatements:false;
		multiLineMessages:Array new;
		oneLineMessages:Array new;
		maxLineLength: 50;
		newLineBeforeFirstKeyword:false;
		keepBlockInMessage:false;
		numberOfArgumentsForMultiLine:1;
		indentsForKeywords: 0
]

{ #category : #configurations }
BIMessageExpressionTest >> oneArgumentPerLineConfiguration [
	"Here we can control explicitly the configuration we want."

	" String streamContents: [:s |
			BIPrettyPrinterContext new storeOn: s]"
	
	^ self contextClass basicNew
		indentString: '	';
		formatCommentWithStatements:false;
		multiLineMessages:Array new;
		oneLineMessages:Array new;
		numberOfArgumentsForMultiLine:1;
		indentsForKeywords: 0;
		maxLineLength: 50;
		newLineBeforeFirstKeyword:false;
		keepBlockInMessage:false
]

{ #category : #configurations }
BIMessageExpressionTest >> oneIdentsForKeyWordConfiguration [
	"Here we can control explicitly the configuration we want."

	" String streamContents: [:s |
			BIPrettyPrinterContext new storeOn: s]"

	^ self contextClass new
		numberOfArgumentsForMultiLine:1;
		indentsForKeywords: 1
]

{ #category : #configurations }
BIMessageExpressionTest >> oneLineMessageConfiguration [

	^ self contextClass basicNew
		indentString: '	';
		formatCommentWithStatements:false;
		multiLineMessages:Array new;
		oneLineMessages:#(#one:line:message:);
		numberOfArgumentsForMultiLine:10;
		indentsForKeywords: 0;
		maxLineLength: 50;
		newLineBeforeFirstKeyword:false;
		keepBlockInMessage:false
]

{ #category : #tests }
BIMessageExpressionTest >> testBinaryFormatParameter [
	| source |
	configurationSelector := #basicConfiguration.
	source := self formatExpression: '1 + (3     factorial)'.
	self assert: source equals: '1 + 3 factorial'
]

{ #category : #tests }
BIMessageExpressionTest >> testBinaryFormatReceiver [
	| source |
	configurationSelector := #basicConfiguration.
	source := self formatExpression: 'a:=''abc'' at: 2'.
	self assert: source equals: 'a := ''abc'' at: 2'
]

{ #category : #tests }
BIMessageExpressionTest >> testBinaryNoExtraSpacesAfter [
	| source |
	configurationSelector := #basicConfiguration.
	source := self formatExpression: '1+2          '.
	self assert: source equals: '1 + 2'
]

{ #category : #tests }
BIMessageExpressionTest >> testBinaryNoExtraSpacesBefore [
	| source |
	configurationSelector := #basicConfiguration.
	source := self formatExpression: '   1+2'.
	self assert: source equals: '1 + 2'
]

{ #category : #tests }
BIMessageExpressionTest >> testBinaryNoExtraSpacesBetween [
	| source |
	configurationSelector := #basicConfiguration.
	source := self formatExpression: '1     +     2'.
	self assert: source equals: '1 + 2'
]

{ #category : #tests }
BIMessageExpressionTest >> testBinaryOneArgumentPerLine [
	| source |
	configurationSelector := #oneArgumentPerLineConfiguration.
	source := self formatExpression: '1+2'.
	self
		assert: source
		equals:
			'1
+ 2'
]

{ #category : #tests }
BIMessageExpressionTest >> testIndentsForKeywords1 [
	| source |
	configurationSelector := #oneIdentsForKeyWordConfiguration.
	source := self formatExpression: 'a k1:1 k2:2 k3:3'.
	self
		assert: source
		equals:
			'a
	k1: 1
	k2: 2
	k3: 3'
]

{ #category : #tests }
BIMessageExpressionTest >> testIndentsForKeywords2 [
	| source |
	configurationSelector := #twoIdentsForKeyWordConfiguration.
	source := self formatExpression: 'a k1:1 k2:2 k3:3'.
	self
		assert: source
		equals:
			'a
		k1: 1
		k2: 2
		k3: 3'
]

{ #category : #tests }
BIMessageExpressionTest >> testKeepBlockInMessageMutlilineNewLine [
	| source |
	configurationSelector := #keepBlockInMessageConfiguration.
	source := self
		formatExpression: 'aBoolean  ifTrue:[ Transcript cr. Transcript cr. Transcript cr]'.
	self
		assert: source
		equals:
			'aBoolean
ifTrue:
	[Transcript cr. Transcript
	cr. Transcript cr]'
]

{ #category : #tests }
BIMessageExpressionTest >> testKeepBlockInMessageMutlilineSpace [
	| source |
	configurationSelector := #keepBlockInMessageConfiguration.
	source := self
		formatExpression:
			'aBoolean ifTrue:[ 12 factorial. 12 factorial. 3
	factorial]'.
	self
		assert: source
		equals:
			'aBoolean
ifTrue: [12 factorial. 12
	factorial. 3
	factorial]'
]

{ #category : #tests }
BIMessageExpressionTest >> testKeepBlockInMessageNotMultilineSpace [
	| source |
	configurationSelector := #keepBlockInMessageConfiguration.
	source := self formatExpression: 'aBoolean ifTrue:[^1]'.
	self
		assert: source
		equals:
			'aBoolean
ifTrue: [^1]'
]

{ #category : #tests }
BIMessageExpressionTest >> testKeepBlockInMessageNotMutlilineNewLine [
	| source |
	configurationSelector := #keepBlockInMessageConfiguration.
	source := self
		formatExpression: 'aBoolean ifTrue:[LongLongLongExpression]'.
	self
		assert: source
		equals:
			'aBoolean
ifTrue:
	[LongLongLongExpression]'
]

{ #category : #tests }
BIMessageExpressionTest >> testKeyWordFormatReceiver [
	| source |
	configurationSelector := #basicConfiguration.
	source := self
		formatExpression: '2 between: 4-2 and: 3     factorial'.
	self assert: source equals: '2 between: 4 - 2 and: 3 factorial'
]

{ #category : #tests }
BIMessageExpressionTest >> testKeywordFormatParameter [
	| source |
	configurationSelector := #basicConfiguration.
	source := self
		formatExpression: '3 between:2      factorial and: 4+5'.
	self assert: source equals: '3 between: 2 factorial and: 4 + 5'
]

{ #category : #tests }
BIMessageExpressionTest >> testKeywordNoExtraSpacesAfter [
	| source |
	configurationSelector := #basicConfiguration.
	source := self formatExpression: '2 between: 1 and: 3          '.
	self assert: source equals: '2 between: 1 and: 3'
]

{ #category : #tests }
BIMessageExpressionTest >> testKeywordNoExtraSpacesBefore [
	| source |
	configurationSelector := #basicConfiguration.
	source := self formatExpression: '   2 between: 1 and: 3'.
	self assert: source equals: '2 between: 1 and: 3'
]

{ #category : #tests }
BIMessageExpressionTest >> testKeywordNoExtraSpacesBetween [
	| source |
	configurationSelector := #basicConfiguration.
	source := self formatExpression: '2      between:     1   and:    3'.
	self assert: source equals: '2 between: 1 and: 3'
]

{ #category : #tests }
BIMessageExpressionTest >> testKeywordOneArgumentPerLine [
	| source |
	configurationSelector := #oneArgumentPerLineConfiguration.
	source := self formatExpression: '2      between:     1   and:    3'.
	self
		assert: source
		equals:
			'2
between: 1
and: 3'
]

{ #category : #tests }
BIMessageExpressionTest >> testMultiLineMessage [
	| source |
	configurationSelector := #multiLineMessageConfiguration.
	source := self formatExpression: 'foo multi:1 line:2 '.
	self
		assert: source
		equals:
			'foo
	multi: 1
	line: 2'
]

{ #category : #tests }
BIMessageExpressionTest >> testNewLineBeforeFirstKeyword [
	| source |
	configurationSelector := #newLineBeforeFirstKeywordConfiguration.
	source := self formatExpression: '2 between: 1 and: 3'.
	self
		assert: source
		equals:
			'2
between: 1 and: 3'
]

{ #category : #tests }
BIMessageExpressionTest >> testNoIndentsForKeywords [
	| source |
	configurationSelector := #noIdentsForKeyWordConfiguration.
	source := self formatExpression: 'a k1:1 k2:2 k3:3'.
	self
		assert: source
		equals:
			'a
k1: 1
k2: 2
k3: 3'
]

{ #category : #tests }
BIMessageExpressionTest >> testOneLineMessage [
	| source |
	configurationSelector := #oneLineMessageConfiguration.
	source := self formatExpression: 'foo one:1 line:2 message:3'.
	self assert: source equals: 'foo one: 1 line: 2 message: 3'
]

{ #category : #tests }
BIMessageExpressionTest >> testTooLongArgument [
	| source |
	configurationSelector := #tooLongArgumentConfiguration.
	source := self formatExpression: 'foo message: TooLongArgument'.
	self
		assert: source
		equals:
			'foo
message:
	TooLongArgument'
]

{ #category : #tests }
BIMessageExpressionTest >> testTraditionalBinaryPrecedence [
	| source |
	configurationSelector := #traditionalBinaryPrecedenceConfiguration.
	source := self formatExpression: '2+3*4'.
	self assert: source equals: '(2 + 3) * 4'
]

{ #category : #tests }
BIMessageExpressionTest >> testTraditionalBinaryPrecedence2 [
	| source |
	configurationSelector := #traditionalBinaryPrecedenceConfiguration.
	source := self formatExpression: '2*3+4'.
	self assert: source equals: '2 * 3 + 4'
]

{ #category : #tests }
BIMessageExpressionTest >> testTraditionalBinaryPrecedence3 [
	| source |
	configurationSelector := #traditionalBinaryPrecedenceConfiguration.
	source := self formatExpression: '2+3*4+5 * 2'.
	self assert: source equals: '((2 + 3) * 4 + 5) * 2'
]

{ #category : #tests }
BIMessageExpressionTest >> testUnaryFormatReceiver [
	| source |
	configurationSelector := #basicConfiguration.
	source := self formatExpression: 'a:=1 factorial'.
	self assert: source equals: 'a := 1 factorial'
]

{ #category : #tests }
BIMessageExpressionTest >> testUnaryNoExtraSpacesAfter [
	| source |
	configurationSelector := #basicConfiguration.
	source := self formatExpression: 'Transcript cr        '.
	self assert: source equals: 'Transcript cr'
]

{ #category : #tests }
BIMessageExpressionTest >> testUnaryNoExtraSpacesBefore [
	| source |
	configurationSelector := #basicConfiguration.
	source := self formatExpression: '   Transcript cr'.
	self assert: source equals: 'Transcript cr'
]

{ #category : #tests }
BIMessageExpressionTest >> testUnaryNoExtraSpacesBetween [
	| source |
	configurationSelector := #basicConfiguration.
	source := self formatExpression: 'Transcript        cr'.
	self assert: source equals: 'Transcript cr'
]

{ #category : #tests }
BIMessageExpressionTest >> testUnaryNoExtraSpacesBetween2 [
	| source |
	configurationSelector := #basicConfiguration.
	source := self
		formatExpression: '2           factorial              factorial'.
	self assert: source equals: '2 factorial factorial'
]

{ #category : #configurations }
BIMessageExpressionTest >> tooLongArgumentConfiguration [

	^ self contextClass basicNew
		indentString: '	';
		formatCommentWithStatements:false;
		multiLineMessages:Array new;
		oneLineMessages:Array new;
		numberOfArgumentsForMultiLine:10;
		indentsForKeywords: 0;
		maxLineLength: 10;
		newLineBeforeFirstKeyword:false;
		keepBlockInMessage:false
]

{ #category : #configurations }
BIMessageExpressionTest >> traditionalBinaryPrecedenceConfiguration [
	"Here we can control explicitly the configuration we want."

	" String streamContents: [:s |
			BIPrettyPrinterContext new storeOn: s]"

	^ self contextClass basicNew
	indentString: '	';
	formatCommentWithStatements:false;
	multiLineMessages:Array new;
	oneLineMessages:Array new;
	numberOfArgumentsForMultiLine:10;
	indentsForKeywords: 0;
	maxLineLength: 50;
	newLineBeforeFirstKeyword:false;
	keepBlockInMessage:false;
	useTraditionalBinaryPrecedenceForParentheses:true;
	traditionalBinaryPrecedenceArray: #(#($| $& $?) #($= $~ $< $>) #($- $+) #($* $/ $% $\) #($@));
	stringInsideParentheses: ''
]

{ #category : #configurations }
BIMessageExpressionTest >> twoIdentsForKeyWordConfiguration [
	"Here we can control explicitly the configuration we want."

	" String streamContents: [:s |
			BIPrettyPrinterContext new storeOn: s]"

	^ self contextClass basicNew
	indentString: '	';
	formatCommentWithStatements:false;
	multiLineMessages:Array new;
	oneLineMessages:Array new;
	numberOfArgumentsForMultiLine:1;
	indentsForKeywords: 2;
	maxLineLength: 50;
	newLineBeforeFirstKeyword:false;
	keepBlockInMessage:false
]

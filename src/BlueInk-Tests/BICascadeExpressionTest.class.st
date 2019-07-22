Class {
	#name : #BICascadeExpressionTest,
	#superclass : #BISingleExpressionTest,
	#category : #'BlueInk-Tests-Base'
}

{ #category : #configurations }
BICascadeExpressionTest >> basicConfiguration [

	^ self contextClass basicNew
		formatCommentWithStatements:true;
		newLineBeforeFirstCascade: false;
		multiLineMessages: Array new;
		oneLineMessages: Array new;
		numberOfArgumentsForMultiLine:3;
		maxLineLength:50;
		keepBlockInMessage:false;
		newLineAfterCascade:false;
		indentString: '	'
]

{ #category : #configurations }
BICascadeExpressionTest >> multilineMessageConfiguration [
	"Here we can control explicitly the configuration we want."

	" String streamContents: [:s |
			BIPrettyPrinterContext new storeOn: s]"

	^ self contextClass basicNew
		newLineAfterCascade: true;
		newLineBeforeFirstCascade: true;
		formatCommentWithStatements:false;
		multiLineMessages:#(#multi:line:);
		oneLineMessages: Array new;
		numberOfArgumentsForMultiLine:3;
		maxLineLength:50;
		keepBlockInMessage:false;
		indentString: '	';
		newLineBeforeFirstKeyword:false;
		stringInsideBlocks: ' ';
		retainBlankLinesBetweenStatements:false;
		minimumNewLinesBetweenStatements:1;
		indentsForKeywords:0;
		periodsAtEndOfBlock:false;
		periodsAtEndOfMethod: false;
		lineUpBlockBrackets:false
]

{ #category : #configurations }
BICascadeExpressionTest >> newLineAfterCascadeConfiguration [
	"Here we can control explicitly the configuration we want."

	" String streamContents: [:s |
			BIPrettyPrinterContext new storeOn: s]"

"	^ self contextClass new

"

	^ self contextClass basicNew
		newLineAfterCascade: true;
		newLineBeforeFirstCascade: false;
		formatCommentWithStatements:false;
		multiLineMessages: Array new;
		oneLineMessages: Array new;
		numberOfArgumentsForMultiLine:3;
		maxLineLength:50;
		keepBlockInMessage:false;
		indentString: '	';
		newLineBeforeFirstKeyword:false
]

{ #category : #configurations }
BICascadeExpressionTest >> newLineBeforeAndAfterCascadeConfiguration [
	"Here we can control explicitly the configuration we want."

	" String streamContents: [:s |
			BIPrettyPrinterContext new storeOn: s]"

	^ self contextClass basicNew
		newLineAfterCascade: true;
		newLineBeforeFirstCascade: true;
		formatCommentWithStatements:false;
		multiLineMessages: Array new;
		oneLineMessages: Array new;
		numberOfArgumentsForMultiLine:3;
		maxLineLength:50;
		keepBlockInMessage:false;
		indentString: '	';
		newLineBeforeFirstKeyword:false
]

{ #category : #configurations }
BICascadeExpressionTest >> newLineBeforeCascadeConfiguration [
	"Here we can control explicitly the configuration we want."

	" String streamContents: [:s |
			BIPrettyPrinterContext new storeOn: s]"		

	^ self contextClass basicNew
		newLineAfterCascade: false;
		newLineBeforeFirstCascade: true;
		formatCommentWithStatements:false;
		multiLineMessages: Array new;
		oneLineMessages: Array new;
		numberOfArgumentsForMultiLine:3;
		maxLineLength:50;
		keepBlockInMessage:false;
		indentString: '	';
		newLineBeforeFirstKeyword:false

]

{ #category : #configurations }
BICascadeExpressionTest >> noNewLineAfterCascadeConfiguration [
	"Here we can control explicitly the configuration we want."

	" String streamContents: [:s |
			BIPrettyPrinterContext new storeOn: s]"

	^ self contextClass basicNew
		newLineAfterCascade: false;
		newLineBeforeFirstCascade: false;
		formatCommentWithStatements:false;
		multiLineMessages: Array new;
		oneLineMessages: Array new;
		numberOfArgumentsForMultiLine:3;
		maxLineLength:50;
		keepBlockInMessage:false;
		indentString: '	';
		newLineBeforeFirstKeyword:false
]

{ #category : #configurations }
BICascadeExpressionTest >> noNewLineBeforeCascadeConfiguration [
	"Here we can control explicitly the configuration we want."

	" String streamContents: [:s |
			BIPrettyPrinterContext new storeOn: s]"

	^ self contextClass basicNew
		newLineAfterCascade: false;
		newLineBeforeFirstCascade: false;
		formatCommentWithStatements:false;
		multiLineMessages: Array new;
		oneLineMessages: Array new;
		numberOfArgumentsForMultiLine:3;
		maxLineLength:50;
		keepBlockInMessage:false;
		indentString: '	';
		newLineBeforeFirstKeyword:false

]

{ #category : #tests }
BICascadeExpressionTest >> testCommentAfterMessages [
	| source |
	configurationSelector := #basicConfiguration.
	source := self
		formatExpression:
			'Transcript show:1 "firstComment";
	cr "secondComment";
	cr "thirdComment"'.
	self
		assert: source
		equals:
			'Transcript show: 1; "secondComment"
		cr; "thirdComment"
		cr'
]

{ #category : #tests }
BICascadeExpressionTest >> testCommentBeforeMessage [
	| source |
	configurationSelector := #basicConfiguration.
	source := self formatExpression: 'Transcript show:1; "cr" cr'.
	self
		assert: source
		equals:
			'Transcript show: 1; "cr"
		cr'
]

{ #category : #tests }
BICascadeExpressionTest >> testFormatMessage [
	| source |
	configurationSelector := #noNewLineAfterCascadeConfiguration.
	source := self formatExpression: '1 *4; +1'.
	self assert: source equals: '1 * 4; + 1'
]

{ #category : #tests }
BICascadeExpressionTest >> testFormatMessageAndArgument [
	| source |
	configurationSelector := #basicConfiguration.
	source := self formatExpression: '1 between:2 and:5; +1'.
	self assert: source equals: '1 between: 2 and: 5; + 1'
]

{ #category : #tests }
BICascadeExpressionTest >> testFormatMultilineMessage [
	| source |
	configurationSelector := #multilineMessageConfiguration.
	source := self
		formatExpression: '1 between:2 and:5; to:4 do:[:each| each factorial. each + 1]'.
	self
		assert: source
		equals:
			'1
	between: 2 and: 5;
	to: 4
		do:
		[ :each | 
		each factorial.
		each + 1 ]'
]

{ #category : #tests }
BICascadeExpressionTest >> testFormatMultilineMessage2 [
	| source |
	configurationSelector := #multilineMessageConfiguration.
	source := self formatExpression: '1 factorial; multi:1 line:2'.
	self
		assert: source
		equals:
			'1
	factorial;
	multi: 1
		line: 2'
]

{ #category : #tests }
BICascadeExpressionTest >> testFormatReceiver [
	| source |
	configurationSelector := #noNewLineAfterCascadeConfiguration.
	source := self formatExpression: 'a:=1 factorial; +1'.
	self assert: source equals: 'a := 1 factorial; + 1'
]

{ #category : #tests }
BICascadeExpressionTest >> testNewLineAfterCascade [
	| source |
	configurationSelector := #newLineAfterCascadeConfiguration.
	source := self formatExpression: 'Foo a;b;c'.
	self
		assert: source
		equals:
			'Foo a;
	b;
	c'
]

{ #category : #tests }
BICascadeExpressionTest >> testNewLineAfterCascade2 [
	| source |
	configurationSelector := #newLineAfterCascadeConfiguration.
	source := self
		formatExpression: 'OrderedCollection new add: #a;add:#b;yourself'.
	self
		assert: source
		equals:
			'OrderedCollection new add: #a;
	add: #b;
	yourself'
]

{ #category : #tests }
BICascadeExpressionTest >> testNewLineBeforeAndAfterCascade [
	| source |
	configurationSelector := #newLineBeforeAndAfterCascadeConfiguration.
	source := self formatExpression: 'Transcript cr;cr;cr'.
	self
		assert: source
		equals:
			'Transcript
	cr;
	cr;
	cr'
]

{ #category : #tests }
BICascadeExpressionTest >> testNewLineBeforeCascade [
	| source |
	configurationSelector := #newLineBeforeCascadeConfiguration.
	source := self formatExpression: 'Foo a;b;c'.
	self
		assert: source
		equals:
			'Foo
	a; b; c'
]

{ #category : #tests }
BICascadeExpressionTest >> testNewLineBeforeCascade2 [
	| source |
	configurationSelector := #newLineBeforeCascadeConfiguration.
	source := self
		formatExpression: 'OrderedCollection new add: #a;add:#b;yourself'.
	self
		assert: source
		equals:
			'OrderedCollection new
	add: #a; add: #b; yourself'
]

{ #category : #tests }
BICascadeExpressionTest >> testNoNewLineAfterCascade [
	| source |
	configurationSelector := #noNewLineAfterCascadeConfiguration.
	source := self formatExpression: 'Foo a;b;c'.
	self assert: source equals: 'Foo a; b; c'
]

{ #category : #tests }
BICascadeExpressionTest >> testNoNewLineAfterCascade2 [
	| source |
	configurationSelector := #noNewLineAfterCascadeConfiguration.
	source := self
		formatExpression: 'OrderedCollection new add: #a;add:#b;yourself'.
	self
		assert: source
		equals: 'OrderedCollection new add: #a; add: #b; yourself'
]

{ #category : #tests }
BICascadeExpressionTest >> testNoNewLineBeforeCascade [
	| source |
	configurationSelector := #noNewLineBeforeCascadeConfiguration.
	source := self formatExpression: 'Foo a;b;c'.
	self assert: source equals: 'Foo a; b; c'
]

{ #category : #tests }
BICascadeExpressionTest >> testNoNewLineBeforeCascade2 [
	| source |
	configurationSelector := #noNewLineBeforeCascadeConfiguration.
	source := self
		formatExpression: 'OrderedCollection new add: #a;add:#b;yourself'.
	self
		assert: source
		equals: 'OrderedCollection new add: #a; add: #b; yourself'
]

{ #category : #tests }
BICascadeExpressionTest >> testTooLongArgument [
	| source |
	configurationSelector := #tooLongArgumentConfiguration.
	source := self formatExpression: 'foo a;b: toLongArgument'.
	self
		assert: source
		equals:
			'foo a; b:
		toLongArgument'
]

{ #category : #configurations }
BICascadeExpressionTest >> tooLongArgumentConfiguration [

	^ self contextClass basicNew
		formatCommentWithStatements:true;
		newLineBeforeFirstCascade: false;
		multiLineMessages: Array new;
		oneLineMessages: Array new;
		numberOfArgumentsForMultiLine:3;
		maxLineLength:10;
		keepBlockInMessage:false;
		newLineAfterCascade:false;
		indentString: '	'
]

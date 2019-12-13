Class {
	#name : #BIMethodeExpressionTest,
	#superclass : #BISingleExpressionTest,
	#category : #'BlueInk-Tests-Base'
}

{ #category : #configurations }
BIMethodeExpressionTest >> basicCommentFormatConfiguration [
	"Here we can control explicitly the configuration we want."

	" String streamContents: [:s |
			BIPrettyPrinterContext new storeOn: s]"

	^ self contextClass basicNew
		indentString: '	';
		formatCommentWithStatements:false;
		useBasicCommentFormat: true;
		newLinesAfterMethodPattern: 1;
		newLinesAfterMethodComment: 1
]

{ #category : #configurations }
BIMethodeExpressionTest >> basicConfiguration [
	"Here we can control explicitly the configuration we want."

	" String streamContents: [:s |
			BIPrettyPrinterContext new storeOn: s]"

	^ self contextClass basicNew
		indentString: '	';
		formatCommentWithStatements:false;
		useBasicCommentFormat: false;
		maxLineLength: 50;
		periodsAtEndOfBlock: false;
		periodsAtEndOfMethod: false;
		newLinesAfterMethodPattern: 1;
		newLinesAfterMethodComment: 1
]

{ #category : #configurations }
BIMethodeExpressionTest >> bigMethodeConfiguration [
	"Here we can control explicitly the configuration we want."

	" String streamContents: [:s |
			BIPrettyPrinterContext new storeOn: s]"

	^ self contextClass basicNew
		indentString: '	';
		formatCommentWithStatements:false;
		useBasicCommentFormat: false;
		maxLineLength: 50;
		periodsAtEndOfBlock: false;
		periodsAtEndOfMethod: false;
		multiLineMessages: Array new;
		oneLineMessages: Array new;
		indentsForKeywords: 1;
		keepBlockInMessage:true;
		retainBlankLinesBetweenStatements:false;
		retainBlankLinesBetweenStatements: false;
		minimumNewLinesBetweenStatements:1;
		newLineBeforeFirstCascade:true;
		newLineAfterCascade:true;
		numberOfArgumentsForMultiLine:4;
		newLinesAfterTemporaries:1;
		stringInsideBlocks: '';
		lineUpBlockBrackets: true;
		newLinesAfterMethodPattern: 1;
		newLinesAfterMethodComment: 1
]

{ #category : #configurations }
BIMethodeExpressionTest >> cascadeConfiguration [
	"Here we can control explicitly the configuration we want."

	" String streamContents: [:s |
			BIPrettyPrinterContext new storeOn: s]"

	^ self contextClass basicNew
		indentString: '	';
		formatCommentWithStatements:false;
		useBasicCommentFormat: false;
		maxLineLength: 50;
		periodsAtEndOfBlock: false;
		periodsAtEndOfMethod: false;
		newLineBeforeFirstCascade:true;
		newLineAfterCascade:true;
		multiLineMessages: Array new;
		oneLineMessages: Array new;
		numberOfArgumentsForMultiLine:10;
		keepBlockInMessage:true;
		retainBlankLinesBetweenStatements:false;
		newLinesAfterMethodPattern:1;
		minimumNewLinesBetweenStatements: 1
]

{ #category : #configurations }
BIMethodeExpressionTest >> newLineAfterSignatureConfiguration [
	"Here we can control explicitly the configuration we want."

	" String streamContents: [:s |
			BIPrettyPrinterContext new storeOn: s]"

	^ self contextClass basicNew
		indentString: '	';
		formatCommentWithStatements:false;
		newLinesAfterMethodPattern: 1
]

{ #category : #configurations }
BIMethodeExpressionTest >> newLineBetweenTopCommentsConfiguration [
	"Here we can control explicitly the configuration we want."

	" String streamContents: [:s |
			BIPrettyPrinterContext new storeOn: s]"

	^ self contextClass basicNew
		indentString: '	';
		formatCommentWithStatements:false;
		useBasicCommentFormat: false;
		maxLineLength: 50;
		periodsAtEndOfBlock: false;
		periodsAtEndOfMethod: false;
		newLinesAfterMethodPattern: 1;
		newLinesAfterMethodComment: 1
]

{ #category : #configurations }
BIMethodeExpressionTest >> noNewLineBetweenTopCommentsConfiguration [
	"Here we can control explicitly the configuration we want."

	" String streamContents: [:s |
			BIPrettyPrinterContext new storeOn: s]"

	^ self contextClass basicNew
		indentString: '	';
		formatCommentWithStatements:false;
		useBasicCommentFormat: true;
		newLinesAfterMethodPattern: 1;
		newLinesAfterMethodComment: 0
]

{ #category : #configurations }
BIMethodeExpressionTest >> noNewLinesAfterCommentConfiguration [
	"Here we can control explicitly the configuration we want."

	" String streamContents: [:s |
			BIPrettyPrinterContext new storeOn: s]"

	^ self contextClass basicNew
		indentString: '	';
		formatCommentWithStatements:false;
		useBasicCommentFormat: true;
		newLinesAfterMethodPattern: 1;
		newLinesAfterMethodComment: 1
]

{ #category : #configurations }
BIMethodeExpressionTest >> notBasicCommentFormatConfiguration [
	"Here we can control explicitly the configuration we want."

	" String streamContents: [:s |
			BIPrettyPrinterContext new storeOn: s]"

	^ self contextClass basicNew
		indentString: '	';
		formatCommentWithStatements:false;
		useBasicCommentFormat: false;
		maxLineLength: 50;	
		newLinesAfterMethodPattern: 1;
		newLinesAfterMethodComment: 1
]

{ #category : #configurations }
BIMethodeExpressionTest >> oneNewLineAfterCommentConfiguration [
	"Here we can control explicitly the configuration we want."

	" String streamContents: [:s |
			BIPrettyPrinterContext new storeOn: s]"

	^ self contextClass basicNew
		indentString: '	';
		formatCommentWithStatements:false;
		useBasicCommentFormat:false;
		maxLineLength:50;
		newLinesAfterMethodPattern: 1;
		newLinesAfterMethodComment: 2
]

{ #category : #configurations }
BIMethodeExpressionTest >> oneNewLinesAfterSignatureConfiguration [
	"Here we can control explicitly the configuration we want."

	" String streamContents: [:s |
			BIPrettyPrinterContext new storeOn: s]"

	^ self contextClass basicNew
		indentString: '	';
		formatCommentWithStatements:false
]

{ #category : #configurations }
BIMethodeExpressionTest >> signatureConfiguration [
	"Here we can control explicitly the configuration we want."

	" String streamContents: [:s |
			BIPrettyPrinterContext new storeOn: s]"

	^ self contextClass basicNew
		indentString: '	';
		formatCommentWithStatements:false;
		newLinesAfterMethodPattern: 1
]

{ #category : #configurations }
BIMethodeExpressionTest >> signatureNotOnMultipleLinesConfiguration [
	"Here we can control explicitly the configuration we want."

	" String streamContents: [:s |
			BIPrettyPrinterContext new storeOn: s]"

	^ self contextClass basicNew
		indentString: '	';
		formatCommentWithStatements:false;
		methodSignatureOnMultipleLines: false;
		newLinesAfterMethodPattern: 1
]

{ #category : #configurations }
BIMethodeExpressionTest >> signatureOnMultipleLinesConfiguration [
	"Here we can control explicitly the configuration we want."

	" String streamContents: [:s |
			BIPrettyPrinterContext new storeOn: s]"

	^ self contextClass basicNew
		indentString: '	';
		formatCommentWithStatements:false;
		methodSignatureOnMultipleLines: true;
		selectorAndArgumentCombinedMaxSize: 2;
		newLinesAfterMethodPattern: 0
]

{ #category : #test }
BIMethodeExpressionTest >> testBasic [
	| expr source |
	expr := self parserClass
		parseMethod: 'aMethode <pragma> "aComment" body'.
	configurationSelector := #basicConfiguration.
	source := self formatter format: expr.
	self
		assert: source
		equals:
			'aMethode
	"aComment"
	<pragma>
	body'
]

{ #category : #test }
BIMethodeExpressionTest >> testBasic2 [
	| expr source |
	expr := self parserClass
		parseMethod: 'aMethode <pragma1> <pragma2> "aComment1" "aComment2"  body'.
	configurationSelector := #newLineBetweenTopCommentsConfiguration.
	source := self formatter format: expr.
	self
		assert: source
		equals:
			'aMethode
	"aComment1"
	"aComment2"
	<pragma1>
	<pragma2>
	body'
]

{ #category : #test }
BIMethodeExpressionTest >> testBigMethode [
	| expr source |
	expr := self parserClass
		parseMethod:
			'aMethode
	"aComment"
	"anotherComment"
	<pragma>
	| tmp |
	tmp := 1.
	tmp to: 10 do: [:i | Transcript show:i ; cr ;cr]
	'.
	configurationSelector := #bigMethodeConfiguration.
	source := self formatter format: expr.
	self
		assert: source
		equals:
			'aMethode
	"aComment"
	"anotherComment"
	<pragma>
	| tmp |
	tmp := 1.
	tmp
		to: 10
		do: [:i | 
			Transcript
				show: i;
				cr;
				cr
			]'
]

{ #category : #test }
BIMethodeExpressionTest >> testCommentBasicFormat [
	| expr source |
	expr := self parserClass parseMethod: 'aMethode "   aComment   "'.
	configurationSelector := #basicCommentFormatConfiguration.
	source := self formatter format: expr.
	self
		assert: source
		equals:
			'aMethode
	"   aComment   "
	'
]

{ #category : #test }
BIMethodeExpressionTest >> testCommentNotBasicFormat [
	| expr source |
	expr := self parserClass parseMethod: 'aMethode "   aComment   "'.
	configurationSelector := #notBasicCommentFormatConfiguration.
	source := self formatter format: expr.
	self
		assert: source
		equals:
			'aMethode
	"aComment   "
	'
]

{ #category : #test }
BIMethodeExpressionTest >> testFormatBody [
	| expr source |
	expr := self parserClass parseMethod: 'aMethode a:=1'.
	configurationSelector := #basicConfiguration.
	source := self formatter format: expr.
	self
		assert: source
		equals:
			'aMethode
	a := 1'
]

{ #category : #test }
BIMethodeExpressionTest >> testIndentCascade [
	| expr source |
	expr := self parserClass
		parseMethod: 'aMethode	Transcript cr; show: ''foo'';	cr. Transcript cr;cr'.
	configurationSelector := #cascadeConfiguration.
	source := self formatter format: expr.
	self
		assert: source
		equals:
			'aMethode
	Transcript
		cr;
		show: ''foo'';
		cr.
	Transcript
		cr;
		cr'
]

{ #category : #test }
BIMethodeExpressionTest >> testNewLinesAfterComment [
	| expr source |
	expr := self parserClass parseMethod: 'aMethode "aComment"'.
	configurationSelector := #oneNewLineAfterCommentConfiguration.
	source := self formatter format: expr.
	self
		assert: source
		equals:
			'aMethode
	"aComment"

	'
]

{ #category : #test }
BIMethodeExpressionTest >> testNewLinesAfterComment2 [
	| expr source |
	expr := self parserClass
		parseMethod: 'aMethode "aComment" "anotherComment"'.
	configurationSelector := #oneNewLineAfterCommentConfiguration.
	source := self formatter format: expr.
	self
		assert: source
		equals:
			'aMethode
	"aComment"

	"anotherComment"

	'
]

{ #category : #test }
BIMethodeExpressionTest >> testNewLinesAfterGluedComment [
	| expr source |
	expr := self parserClass
		parseMethod: 'aMethode"aComment""anotherComment"'.
	configurationSelector := #oneNewLineAfterCommentConfiguration.
	source := self formatter format: expr.
	self
		assert: source
		equals:
			'aMethode
	"aComment""anotherComment"

	'
]

{ #category : #test }
BIMethodeExpressionTest >> testNewLinesAfterSignature [
	| expr source |
	expr := self parserClass parseMethod: 'aMethode'.
	configurationSelector := #newLineAfterSignatureConfiguration.
	source := self formatter format: expr.
	self
		assert: source
		equals:
			'aMethode
	'
]

{ #category : #test }
BIMethodeExpressionTest >> testNewLinesBetweenTopComments [
	| expr source |
	expr := self parserClass
		parseMethod: 'aMethode "aComment" "anotherComment"'.
	configurationSelector := #newLineBetweenTopCommentsConfiguration.
	source := self formatter format: expr.
	self
		assert: source
		equals:
			'aMethode
	"aComment"
	"anotherComment"
	'
]

{ #category : #test }
BIMethodeExpressionTest >> testNoNewLinesAfterComment [
	| expr source |
	expr := self parserClass parseMethod: 'aMethode "aComment"'.
	configurationSelector := #basicCommentFormatConfiguration.
	source := self formatter format: expr.
	self
		assert: source
		equals:
			'aMethode
	"aComment"
	'
]

{ #category : #test }
BIMethodeExpressionTest >> testNoNewLinesAfterGluedComment [
	| expr source |
	expr := self parserClass
		parseMethod: 'aMethode "aComment""anotherComment"'.
	configurationSelector := #noNewLinesAfterCommentConfiguration.
	source := self formatter format: expr.
	self
		assert: source
		equals:
			'aMethode
	"aComment""anotherComment"
	'
]

{ #category : #test }
BIMethodeExpressionTest >> testNoNewLinesAfterSignature [
	| expr source |
	expr := self parserClass parseMethod: 'aMethode'.
	configurationSelector := #signatureConfiguration.
	source := self formatter format: expr.
	self
		assert: source
		equals:
			'aMethode
	'
]

{ #category : #test }
BIMethodeExpressionTest >> testNoNewLinesBetweenTopComments [
	| expr source |
	expr := self parserClass
		parseMethod: 'aMethode "aComment" "anotherComment"'.
	configurationSelector := #noNewLineBetweenTopCommentsConfiguration.
	source := self formatter format: expr.
	self
		assert: source
		equals:
			'aMethode
	"aComment"	"anotherComment"	'
]

{ #category : #test }
BIMethodeExpressionTest >> testOneNewLinesAfterSignature [
	| expr source |
	expr := self parserClass parseMethod: 'aMethode'.
	configurationSelector := #oneNewLineAfterCommentConfiguration.
	source := self formatter format: expr.
	self
		assert: source
		equals:
			'aMethode
	'
]

{ #category : #test }
BIMethodeExpressionTest >> testPragma [
	| expr source |
	expr := self parserClass parseMethod: 'aMethode <pragma>'.
	configurationSelector := #notBasicCommentFormatConfiguration.
	source := self formatter format: expr.
	self
		assert: source
		equals:
			'aMethode
	<pragma>
	'
]

{ #category : #test }
BIMethodeExpressionTest >> testSignatureNotOnMultipleLines [
	| expr source |
	expr := self parserClass parseMethod: 'aMethode:arg1 foo:arg2'.
	configurationSelector := #signatureNotOnMultipleLinesConfiguration.
	source := self formatter format: expr.
	self
		assert: source
		equals:
			'aMethode: arg1 foo: arg2
	'
]

{ #category : #test }
BIMethodeExpressionTest >> testSignatureOnMultipleLines [
	| expr source |
	expr := self parserClass parseMethod: 'aMethode: arg1 foo:arg2'.
	configurationSelector := #signatureOnMultipleLinesConfiguration.
	source := self formatter format: expr.
	self
		assert: source
		equals:
			'aMethode: arg1
	foo: arg2
	'
]

{ #category : #test }
BIMethodeExpressionTest >> testSignatureWithArgument [
	| expr source |
	expr := self parserClass parseMethod: 'aMethode:arg'.
	configurationSelector := #signatureNotOnMultipleLinesConfiguration.
	source := self formatter format: expr.
	self
		assert: source
		equals:
			'aMethode: arg
	'
]

{ #category : #test }
BIMethodeExpressionTest >> testSignatureWithoutArgument [
	| expr source |
	expr := self parserClass parseMethod: 'aMethode'.
	configurationSelector := #basicConfiguration.
	source := self formatter format: expr.
	self
		assert: source
		equals:
			'aMethode
	'
]

{ #category : #test }
BIMethodeExpressionTest >> testTwoNewLinesAfterSignature [
	| expr source |
	expr := self parserClass parseMethod: 'aMethode'.
	configurationSelector := #twoNewLinesAfterSignatureConfiguration.
	source := self formatter format: expr.
	self
		assert: source
		equals:
			'aMethode


	'
]

{ #category : #configurations }
BIMethodeExpressionTest >> twoNewLinesAfterSignatureConfiguration [
	"Here we can control explicitly the configuration we want."

	" String streamContents: [:s |
			BIPrettyPrinterContext new storeOn: s]"

	^ self contextClass basicNew
		indentString: '	';
		formatCommentWithStatements:false;
		newLinesAfterMethodPattern: 3
]

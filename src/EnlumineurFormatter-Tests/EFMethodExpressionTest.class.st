Class {
	#name : #EFMethodExpressionTest,
	#superclass : #EFTest,
	#category : #'EnlumineurFormatter-Tests'
}

{ #category : #configurations }
EFMethodExpressionTest >> basicCommentFormatConfiguration [
	"Here we can control explicitely the configuration we want."

	" String streamContents: [:s |
			BIPrettyPrinterContext new storeOn: s]"

	^ self contextClass basicNew
		indentStyle: #tabulation;
		formatCommentCloseToStatements:false;
		useBasicCommentFormat: true;
		numberOfNewLinesAfterMethodSignature: 1;
		numberOfNewLinesAfterMethodComment: 1;
		indentExtraSpaces: 0
]

{ #category : #configurations }
EFMethodExpressionTest >> basicConfiguration [
	"Here we can control explicitely the configuration we want."

	" String streamContents: [:s |
			BIPrettyPrinterContext new storeOn: s]"

	^ self contextClass basicNew
		indentStyle: #tabulation;
		formatCommentCloseToStatements:false;
		useBasicCommentFormat: false;
		maxLineLength: 50;
		periodAtEndOfBlock: false;
		periodAtEndOfMethod: false;
		numberOfNewLinesAfterMethodSignature: 1;
		numberOfNewLinesAfterMethodComment: 1;
		indentExtraSpaces:0
]

{ #category : #configurations }
EFMethodExpressionTest >> bigMethodeConfiguration [
	"Here we can control explicitely the configuration we want."

	" String streamContents: [:s |
			BIPrettyPrinterContext new storeOn: s]"

	^ self contextClass basicNew
		indentStyle: #tabulation;
		formatCommentCloseToStatements: false;
		useBasicCommentFormat: false;
		maxLineLength: 50;
		periodAtEndOfBlock: false;
		periodAtEndOfMethod: false;
		multiLineMessages: Array new;
		oneLineMessages: Array new;
		indentsForKeywords: 1;
		keepBlockInMessage: true;
		retainBlankLinesBetweenStatements: false;
		retainBlankLinesBetweenStatements: false;
		minimumNewLinesBetweenStatements: 1;
		newLineBeforeFirstCascade: true;
		newLineAfterCascade: true;
		numberOfArgumentsForMultiLine: 4;
		numberOfNewLinesAfterTemporaries: 1;
		numberOfSpacesInsideBlock: 0;
		lineUpBlockBrackets: true;
		numberOfNewLinesAfterMethodSignature: 1;
		numberOfNewLinesAfterMethodComment: 1;
		newLineBeforeFirstKeyword:true;
		indentExtraSpaces:0
]

{ #category : #configurations }
EFMethodExpressionTest >> cascadeConfiguration [
	"Here we can control explicitely the configuration we want."

	" String streamContents: [:s |
			BIPrettyPrinterContext new storeOn: s]"

	^ self contextClass basicNew
		indentStyle: #tabulation;
		formatCommentCloseToStatements:false;
		useBasicCommentFormat: false;
		maxLineLength: 50;
		periodAtEndOfBlock: false;
		periodAtEndOfMethod: false;
		newLineBeforeFirstCascade:true;
		newLineAfterCascade:true;
		multiLineMessages: Array new;
		oneLineMessages: Array new;
		numberOfArgumentsForMultiLine:10;
		keepBlockInMessage:true;
		retainBlankLinesBetweenStatements:false;
		numberOfNewLinesAfterMethodSignature:1;
		minimumNewLinesBetweenStatements: 1;
		indentExtraSpaces:0
]

{ #category : #configurations }
EFMethodExpressionTest >> newLineBetweenTopCommentsConfiguration [
	"Here we can control explicitely the configuration we want."

	" String streamContents: [:s |
			BIPrettyPrinterContext new storeOn: s]"

	^ self contextClass basicNew
		indentStyle: #tabulation;
		formatCommentCloseToStatements:false;
		useBasicCommentFormat: false;
		maxLineLength: 50;
		periodAtEndOfBlock: false;
		periodAtEndOfMethod: false;
		numberOfNewLinesAfterMethodSignature: 1;
		numberOfNewLinesAfterMethodComment: 1;
		indentExtraSpaces:0
]

{ #category : #configurations }
EFMethodExpressionTest >> noNewLineBetweenTopCommentsConfiguration [
	"Here we can control explicitely the configuration we want."

	" String streamContents: [:s |
			BIPrettyPrinterContext new storeOn: s]"

	^ self contextClass basicNew
		indentStyle: #tabulation;
		formatCommentCloseToStatements:false;
		useBasicCommentFormat: true;
		numberOfNewLinesAfterMethodSignature: 1;
		numberOfNewLinesAfterMethodComment: 0;
		indentExtraSpaces:0
]

{ #category : #configurations }
EFMethodExpressionTest >> noNewLinesAfterCommentConfiguration [
	"Here we can control explicitely the configuration we want."

	" String streamContents: [:s |
			BIPrettyPrinterContext new storeOn: s]"

	^ self contextClass basicNew
		indentStyle: #tabulation;
		formatCommentCloseToStatements:false;
		useBasicCommentFormat: true;
		numberOfNewLinesAfterMethodSignature: 1;
		numberOfNewLinesAfterMethodComment: 1;
		indentExtraSpaces:0
]

{ #category : #configurations }
EFMethodExpressionTest >> notBasicCommentFormatConfiguration [
	"Here we can control explicitely the configuration we want."

	" String streamContents: [:s |
			BIPrettyPrinterContext new storeOn: s]"

	^ self contextClass basicNew
		indentStyle: #tabulation;
		formatCommentCloseToStatements:false;
		useBasicCommentFormat: false;
		maxLineLength: 50;	
		numberOfNewLinesAfterMethodSignature: 1;
		numberOfNewLinesAfterMethodComment: 1;
		indentExtraSpaces:0
]

{ #category : #configurations }
EFMethodExpressionTest >> oneNewLineAfterCommentConfiguration [
	"Here we can control explicitely the configuration we want."

	" String streamContents: [:s |
			BIPrettyPrinterContext new storeOn: s]"

	^ self contextClass basicNew
		indentStyle: #tabulation;
		formatCommentCloseToStatements:false;
		useBasicCommentFormat:false;
		maxLineLength:50;
		numberOfNewLinesAfterMethodSignature: 1;
		numberOfNewLinesAfterMethodComment: 2;
		indentExtraSpaces:0
]

{ #category : #configurations }
EFMethodExpressionTest >> oneNewLinesAfterSignatureConfiguration [
	"Here we can control explicitely the configuration we want."

	" String streamContents: [:s |
			BIPrettyPrinterContext new storeOn: s]"

	^ self contextClass basicNew
		indentStyle: #tabulation;
		formatCommentCloseToStatements:false
]

{ #category : #configurations }
EFMethodExpressionTest >> signatureConfiguration [
	"Here we can control explicitely the configuration we want."

	" String streamContents: [:s |
			BIPrettyPrinterContext new storeOn: s]"

	^ self contextClass basicNew
		indentStyle: #tabulation;
		formatCommentCloseToStatements:false;
		numberOfNewLinesAfterMethodSignature: 1;
		indentExtraSpaces:0
]

{ #category : #configurations }
EFMethodExpressionTest >> signatureNotOnMultipleLinesConfiguration [
	"Here we can control explicitely the configuration we want."

	" String streamContents: [:s |
			BIPrettyPrinterContext new storeOn: s]"

	^ self contextClass basicNew
		indentStyle: #tabulation;
		formatCommentCloseToStatements:false;
		methodSignatureOnMultipleLines: false;
		numberOfNewLinesAfterMethodSignature: 1;
		indentExtraSpaces:0
]

{ #category : #configurations }
EFMethodExpressionTest >> signatureOnMultipleLinesConfiguration [
	"Here we can control explicitely the configuration we want."

	" String streamContents: [:s |
			BIPrettyPrinterContext new storeOn: s]"

	^ self contextClass basicNew
		indentStyle: #tabulation;
		formatCommentCloseToStatements:false;
		methodSignatureOnMultipleLines: true;
		selectorAndArgumentCombinedMaxSize: 2;
		numberOfNewLinesAfterMethodSignature: 0;
		indentExtraSpaces:0
]

{ #category : #configurations }
EFMethodExpressionTest >> spaceIndentConfiguration [
	"Here we can control explicitely the configuration we want."

	" String streamContents: [:s |
			BIPrettyPrinterContext new storeOn: s]"

	^ self contextClass basicNew
		numberOfSpacesInIndent: 10; "pay attention since there are dependency."
		indentStyle: #space;
		formatCommentCloseToStatements:false;
		useBasicCommentFormat: false;
		maxLineLength: 50;
		periodAtEndOfBlock: false;
		periodAtEndOfMethod: false;
		numberOfNewLinesAfterMethodSignature: 1;
		numberOfNewLinesAfterMethodComment: 1;
		indentExtraSpaces:0
]

{ #category : #tests }
EFMethodExpressionTest >> testBasic [
	| expr source |
	expr := RBParser parseMethod: 'aMethod <pragma> "aComment" body'.
	configurationSelector := #basicConfiguration.
	source := self formatter format: expr.
	self assert: source equals:
'aMethod
	"aComment"
	<pragma>
	body'
]

{ #category : #tests }
EFMethodExpressionTest >> testBasic2 [
	| expr source |
	expr := RBParser parseMethod: 'aMethod <pragma1> <pragma2> "aComment1" "aComment2"  body'.
	configurationSelector := #newLineBetweenTopCommentsConfiguration.
	source := self formatter format: expr.
	self assert: source equals:
'aMethod
	"aComment1"
	"aComment2"
	<pragma1>
	<pragma2>
	body'
]

{ #category : #tests }
EFMethodExpressionTest >> testBasicSpaceIndent [
	| expr source |
	expr := RBParser parseMethod: 'aMethod <pragma> "aComment" body'.
	configurationSelector := #spaceIndentConfiguration.
	source := self formatter format: expr.
	self assert: source equals:
'aMethod
          "aComment"
          <pragma>
          body'
]

{ #category : #tests }
EFMethodExpressionTest >> testBigMethod [
	| expr source |
	expr := RBParser parseMethod: 
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
	self assert: source equals:
'aMethode
	"aComment"
	"anotherComment"
	<pragma>
	| tmp |
	tmp := 1.
	tmp to: 10 do: [:i | 
		Transcript
			show: i;
			cr;
			cr
		]'
]

{ #category : #tests }
EFMethodExpressionTest >> testCommentBasicFormat [
	| expr source |
	expr := RBParser parseMethod: 'aMethode "   aComment   "'.
	configurationSelector := #basicCommentFormatConfiguration.
	source := self formatter format: expr.
	self assert: source equals:
'aMethode
	"   aComment   "
	'
]

{ #category : #tests }
EFMethodExpressionTest >> testCommentNotBasicFormat [
	| expr source |
	expr := RBParser parseMethod: 'aMethode "   aComment   "'.
	configurationSelector := #notBasicCommentFormatConfiguration.
	source := self formatter format: expr.
	self assert: source equals:
'aMethode
	"aComment   "
	'
]

{ #category : #tests }
EFMethodExpressionTest >> testFormatBody [
	| expr source |
	expr := RBParser parseMethod: 'aMethode a:=1'.
	configurationSelector := #basicConfiguration.
	source := self formatter format: expr.
	self assert: source equals:
'aMethode
	a := 1'
]

{ #category : #tests }
EFMethodExpressionTest >> testIndentCascade [
	| expr source |
	expr := RBParser parseMethod: 'aMethode	Transcript cr; show: ''foo'';	cr. Transcript cr;cr'.
	configurationSelector := #cascadeConfiguration.
	source := self formatter format: expr.
	self assert: source equals:
'aMethode
	Transcript
		cr;
		show: ''foo'';
		cr.
	Transcript
		cr;
		cr'
]

{ #category : #tests }
EFMethodExpressionTest >> testMethodWithMessageArgument [
	| expr source |
	expr := RBParser parseMethod: 
'aMethod

	1 to: 10 do: [:i | Transcript show:i ; cr ;cr]
	'.
	configurationSelector := #bigMethodeConfiguration.
	source := self formatter format: expr.
	self assert: source equals:
'aMethod
	1 to: 10 do: [:i | 
		Transcript
			show: i;
			cr;
			cr
		]'
]

{ #category : #tests }
EFMethodExpressionTest >> testNewLinesAfterComment [
	| expr source |
	expr := RBParser parseMethod: 'aMethode "aComment"'.
	configurationSelector := #oneNewLineAfterCommentConfiguration.
	source := self formatter format: expr.
	self assert: source equals:
'aMethode
	"aComment"

	'
]

{ #category : #tests }
EFMethodExpressionTest >> testNewLinesAfterComment2 [
	| expr source |
	expr := RBParser parseMethod: 'aMethode "aComment" "anotherComment"'.
	configurationSelector := #oneNewLineAfterCommentConfiguration.
	source := self formatter format: expr.
	self assert: source equals:
'aMethode
	"aComment"

	"anotherComment"

	'
]

{ #category : #tests }
EFMethodExpressionTest >> testNewLinesAfterGluedComment [
	| expr source |
	expr := RBParser parseMethod: 'aMethode"aComment""anotherComment"'.
	configurationSelector := #oneNewLineAfterCommentConfiguration.
	source := self formatter format: expr.
	self assert: source equals:
'aMethode
	"aComment""anotherComment"

	'
]

{ #category : #tests }
EFMethodExpressionTest >> testNewLinesBetweenTopComments [
	| expr source |
	expr := RBParser parseMethod: 'aMethode "aComment" "anotherComment"'.
	configurationSelector := #newLineBetweenTopCommentsConfiguration.
	source := self formatter format: expr.
	self assert: source equals:
'aMethode
	"aComment"
	"anotherComment"
	'
]

{ #category : #tests }
EFMethodExpressionTest >> testNoNewLinesAfterComment [
	| expr source |
	expr := RBParser parseMethod: 'aMethode "aComment"'.
	configurationSelector := #basicCommentFormatConfiguration.
	source := self formatter format: expr.
	self assert: source equals:
'aMethode
	"aComment"
	'

]

{ #category : #tests }
EFMethodExpressionTest >> testNoNewLinesAfterGluedComment [
	| expr source |
	expr := RBParser parseMethod: 'aMethode "aComment""anotherComment"'.
	configurationSelector := #noNewLinesAfterCommentConfiguration.
	source := self formatter format: expr.
	self assert: source equals:
'aMethode
	"aComment""anotherComment"
	'

]

{ #category : #tests }
EFMethodExpressionTest >> testNoNewLinesAfterSignature [
	| expr source |
	expr := RBParser parseMethod: 'aMethode'.
	configurationSelector := #signatureConfiguration.
	source := self formatter format: expr.
	self assert: source equals:
'aMethode
	'
]

{ #category : #tests }
EFMethodExpressionTest >> testNoNewLinesBetweenTopComments [
	| expr source |
	expr := RBParser parseMethod: 'aMethode "aComment" "anotherComment"'.
	configurationSelector := #noNewLineBetweenTopCommentsConfiguration.
	source := self formatter format: expr.
	self assert: source equals:
'aMethode
	"aComment"	"anotherComment"	'

]

{ #category : #tests }
EFMethodExpressionTest >> testOneNewLinesAfterSignature [
	| expr source |
	expr := RBParser parseMethod: 'aMethode'.
	configurationSelector := #oneNewLineAfterCommentConfiguration.
	source := self formatter format: expr.
	self assert: source equals:
'aMethode
	'
]

{ #category : #tests }
EFMethodExpressionTest >> testPragma [
	| expr source |
	expr := RBParser parseMethod: 'aMethode <pragma>'.
	configurationSelector := #notBasicCommentFormatConfiguration.
	source := self formatter format: expr.
	self assert: source equals:
'aMethode
	<pragma>
	'
]

{ #category : #tests }
EFMethodExpressionTest >> testSignatureNotOnMultipleLines [
	| expr source |
	expr := RBParser parseMethod: 'aMethode:arg1 foo:arg2'.
	configurationSelector := #signatureNotOnMultipleLinesConfiguration.
	source := self formatter format: expr.
	self assert: source equals:
'aMethode: arg1 foo: arg2
	'
]

{ #category : #tests }
EFMethodExpressionTest >> testSignatureOnMultipleLines [
	| expr source |
	expr := RBParser parseMethod: 'aMethode: arg1 foo:arg2'.
	configurationSelector := #signatureOnMultipleLinesConfiguration.
	source := self formatter format: expr.
	self assert: source equals:
'aMethode: arg1
	foo: arg2
	'
]

{ #category : #tests }
EFMethodExpressionTest >> testSignatureWithArgument [
	| expr source |
	expr := RBParser parseMethod: 'aMethode:arg'.
	configurationSelector := #signatureNotOnMultipleLinesConfiguration.
	source := self formatter format: expr.
	self assert: source equals:
'aMethode: arg
	'
]

{ #category : #tests }
EFMethodExpressionTest >> testSignatureWithoutArgument [
	| expr source |
	expr := RBParser parseMethod: 'aMethode'.
	configurationSelector := #basicConfiguration.
	source := self formatter format: expr.
	self assert: source equals:
'aMethode
	'
]

{ #category : #tests }
EFMethodExpressionTest >> testTwoNewLinesAfterSignature [
	| expr source |
	expr := RBParser parseMethod: 'aMethode'.
	configurationSelector := #twoNewLinesAfterSignatureConfiguration.
	source := self formatter format: expr.
	self assert: source equals:
'aMethode


	'
]

{ #category : #configurations }
EFMethodExpressionTest >> twoNewLinesAfterSignatureConfiguration [
	"Here we can control explicitely the configuration we want."

	" String streamContents: [:s |
			BIPrettyPrinterContext new storeOn: s]"

	^ self contextClass basicNew
		indentStyle: #tabulation;
		formatCommentCloseToStatements:false;
		numberOfNewLinesAfterMethodSignature: 3;
		indentExtraSpaces:0
]

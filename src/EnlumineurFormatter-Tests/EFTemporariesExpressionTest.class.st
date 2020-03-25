Class {
	#name : #EFTemporariesExpressionTest,
	#superclass : #EFTest,
	#category : #'EnlumineurFormatter-Tests'
}

{ #category : #configurations }
EFTemporariesExpressionTest >> basicCommentFormatConfiguration [
	"Here we can control explicitely the configuration we want."

	" String streamContents: [:s |
			BIPrettyPrinterContext new storeOn: s]"

	^ self contextClass basicNew
	useBasicCommentFormat: true;
	formatCommentCloseToStatements:true;
	indentStyle: #tabulation;
	numberOfNewLinesAfterTemporaries:0;
	retainBlankLinesBeforeComments:false;
	indentExtraSpaces:0
]

{ #category : #configurations }
EFTemporariesExpressionTest >> commentWithStatementsConfiguration [

	^ self contextClass basicNew
		formatCommentCloseToStatements:true;
		indentStyle: #tabulation;
		numberOfNewLinesAfterTemporaries:0;
		retainBlankLinesBeforeComments:false;
		useBasicCommentFormat:false;
		maxLineLength:50;
		indentExtraSpaces:0
]

{ #category : #configurations }
EFTemporariesExpressionTest >> dontRetainBlankLinesBeforeComments [
	"Here we can control explicitely the configuration we want."

	" String streamContents: [:s |
			BIPrettyPrinterContext new storeOn: s]"

	^ self contextClass basicNew
		retainBlankLinesBeforeComments: false;
		useBasicCommentFormat: false;
		formatCommentCloseToStatements:false;
		indentStyle: #tabulation;
		numberOfNewLinesAfterTemporaries:0;
		indentExtraSpaces:0
]

{ #category : #configurations }
EFTemporariesExpressionTest >> newLinesAfterTemporariesConfiguration [
	"Here we can control explicitely the configuration we want."

	" String streamContents: [:s |
			BIPrettyPrinterContext new storeOn: s]"
		
	^ self contextClass basicNew
		numberOfNewLinesAfterTemporaries: 1;
		useBasicCommentFormat: false;
		formatCommentCloseToStatements:false;
		indentStyle: #tabulation;
		retainBlankLinesBeforeComments:false;
		indentExtraSpaces:0
]

{ #category : #configurations }
EFTemporariesExpressionTest >> noNewLinesAfterTemporariesConfiguration [
	"Here we can control explicitely the configuration we want."

	" String streamContents: [:s |
			BIPrettyPrinterContext new storeOn: s]"

	^ self contextClass basicNew
		numberOfNewLinesAfterTemporaries: 0;
		useBasicCommentFormat: false;
		formatCommentCloseToStatements:false;
		indentStyle: #tabulation;
		retainBlankLinesBeforeComments:false;
		indentExtraSpaces:0
]

{ #category : #configurations }
EFTemporariesExpressionTest >> notBasicCommentFormatConfiguration [
	"Here we can control explicitely the configuration we want."

	" String streamContents: [:s |
			BIPrettyPrinterContext new storeOn: s]"

	^ self contextClass basicNew
		useBasicCommentFormat: false;
		formatCommentCloseToStatements:true;
		indentStyle: #tabulation;
		numberOfNewLinesAfterTemporaries:0;
		retainBlankLinesBeforeComments:false;
		maxLineLength: 50;
		indentExtraSpaces:0
]

{ #category : #configurations }
EFTemporariesExpressionTest >> retainBlankLinesBeforeComments [
	"Here we can control explicitely the configuration we want."

	" String streamContents: [:s |
			BIPrettyPrinterContext new storeOn: s]"
	^ self contextClass basicNew
		numberOfNewLinesAfterTemporaries: 1;
		useBasicCommentFormat: false;
		formatCommentCloseToStatements:true;
		maxLineLength:50;
		indentStyle: #tabulation;
		retainBlankLinesBeforeComments:true;
		indentExtraSpaces:0
]

{ #category : #tests }
EFTemporariesExpressionTest >> testCommentBasicFormat [
	| source |
	configurationSelector := #basicCommentFormatConfiguration.
	source := self formatExpression: ' | d "    comment     " e |'.
	self assert: source equals: '| d "    comment     " e |'
]

{ #category : #tests }
EFTemporariesExpressionTest >> testCommentNotBasicFormat [
	| source |
	configurationSelector := #notBasicCommentFormatConfiguration.
	source := self formatExpression: ' | d "    comment     " e |'.
	self
		assert: source
		equals:
			'| d "comment     
" e |'
]

{ #category : #tests }
EFTemporariesExpressionTest >> testDontRetainBlankLinesBeforeComments [
	| source |
	configurationSelector := #dontRetainBlankLinesBeforeComments.
	source := self
		formatExpression:
			' | d 
	"comment"e |'.
	self
		assert: source
		equals:
			'| d "comment" e |'
]

{ #category : #tests }
EFTemporariesExpressionTest >> testDontRetainBlankLinesBeforeComments2 [
	| source |
	configurationSelector := #dontRetainBlankLinesBeforeComments.
	source := self
		formatExpression:
			' | d 
	
	"comment"
	e |'.
	self
		assert: source
		equals: '| d "comment" e |'
]

{ #category : #tests }
EFTemporariesExpressionTest >> testNewLineAfterTemporaries [
	| source |
	configurationSelector := #newLinesAfterTemporariesConfiguration.
	source := self formatExpression: ' |d e|'.
	self
		assert: source
		equals: '| d e |
'
]

{ #category : #tests }
EFTemporariesExpressionTest >> testNoNewLineAfterTemporaries [
	| source |
	configurationSelector := #noNewLinesAfterTemporariesConfiguration.
	source := self formatExpression: ' |d e|'.
	self assert: source equals: '| d e |'
]

{ #category : #tests }
EFTemporariesExpressionTest >> testRetainBlankLinesBeforeComments [
	| source |
	configurationSelector := #retainBlankLinesBeforeComments.
	source := self
		formatExpression:
			' | d 
	"comment"e |'.
	self
		assert: source
		equals:
			'| d
"comment" e |
'
]

{ #category : #tests }
EFTemporariesExpressionTest >> testRetainBlankLinesBeforeComments2 [
	| source |
	configurationSelector := #retainBlankLinesBeforeComments.
	source := self
		formatExpression:
			' | d 
	
	"comment"e |'.
	self
		assert: source
		equals:
			'| d

"comment" e |
'
]

{ #category : #tests }
EFTemporariesExpressionTest >> testRetainBlankLinesBeforeComments3 [
	| source |
	configurationSelector := #retainBlankLinesBeforeComments.
	source := self formatExpression: ' | d "comment" e |'.
	self
		assert: source
		equals:
			'| d "comment" e |
'
]

{ #category : #tests }
EFTemporariesExpressionTest >> testSpaceBetweenTemporaries [
	| source |
	configurationSelector := #noNewLinesAfterTemporariesConfiguration.
	source := self formatExpression: '|a|'.
	self assert: source equals: '| a |'
]

{ #category : #tests }
EFTemporariesExpressionTest >> testSpaceBetweenTemporaries2 [
	| source |
	configurationSelector := #noNewLinesAfterTemporariesConfiguration.
	source := self formatExpression: '|a b|'.
	self assert: source equals: '| a b |'
]

{ #category : #tests }
EFTemporariesExpressionTest >> testSpaceBetweenVarAndComment [
	| source |
	configurationSelector := #commentWithStatementsConfiguration.
	source := self formatExpression: ' | d "comment" |'.
	self assert: source equals: '| d "comment" |'
]

{ #category : #tests }
EFTemporariesExpressionTest >> testTwoLinesAfterTemporaries [
	| source |
	configurationSelector := #twoLinesAfterTemporariesConfiguration.
	source := self formatExpression: ' |d e|'.
	self
		assert: source
		equals:
			'| d e |

'
]

{ #category : #configurations }
EFTemporariesExpressionTest >> twoLinesAfterTemporariesConfiguration [
	"Here we can control explicitely the configuration we want."

	" String streamContents: [:s |
			BIPrettyPrinterContext new storeOn: s]"
		
	^ self contextClass basicNew
		numberOfNewLinesAfterTemporaries: 2;
		useBasicCommentFormat: false;
		formatCommentCloseToStatements:false;
		indentStyle: #tabulation;
		retainBlankLinesBeforeComments:false;
		indentExtraSpaces:0
]

Class {
	#name : #BITemporariesExpressionTest,
	#superclass : #BISingleExpressionTest,
	#category : #'BlueInk-Tests-Base'
}

{ #category : #configurations }
BITemporariesExpressionTest >> basicCommentFormatConfiguration [
	"Here we can control explicitly the configuration we want."

	" String streamContents: [:s |
			BIPrettyPrinterContext new storeOn: s]"

	^ self contextClass basicNew
	useBasicCommentFormat: true;
	formatCommentWithStatements:true;
	indentString: '	';
	newLinesAfterTemporaries:0;
	retainBlankLinesBeforeComments:false
]

{ #category : #configurations }
BITemporariesExpressionTest >> commentWithStatementsConfiguration [

	^ self contextClass basicNew
		formatCommentWithStatements:true;
		indentString: '	';
		newLinesAfterTemporaries:0;
		retainBlankLinesBeforeComments:false;
		useBasicCommentFormat:false;
		maxLineLength:50
]

{ #category : #configurations }
BITemporariesExpressionTest >> dontRetainBlankLinesBeforeComments [
	"Here we can control explicitly the configuration we want."

	" String streamContents: [:s |
			BIPrettyPrinterContext new storeOn: s]"

	^ self contextClass basicNew
		retainBlankLinesBeforeComments: false;
		useBasicCommentFormat: false;
		formatCommentWithStatements:false;
		indentString: '	';
		newLinesAfterTemporaries:0
]

{ #category : #configurations }
BITemporariesExpressionTest >> newLinesAfterTemporariesConfiguration [
	"Here we can control explicitly the configuration we want."

	" String streamContents: [:s |
			BIPrettyPrinterContext new storeOn: s]"
		
	^ self contextClass basicNew
		newLinesAfterTemporaries: 1;
		useBasicCommentFormat: false;
		formatCommentWithStatements:false;
		indentString: '	';
		retainBlankLinesBeforeComments:false
]

{ #category : #configurations }
BITemporariesExpressionTest >> noNewLinesAfterTemporariesConfiguration [
	"Here we can control explicitly the configuration we want."

	" String streamContents: [:s |
			BIPrettyPrinterContext new storeOn: s]"

	^ self contextClass basicNew
		newLinesAfterTemporaries: 0;
		useBasicCommentFormat: false;
		formatCommentWithStatements:false;
		indentString: '	';
		retainBlankLinesBeforeComments:false
]

{ #category : #configurations }
BITemporariesExpressionTest >> notBasicCommentFormatConfiguration [
	"Here we can control explicitly the configuration we want."

	" String streamContents: [:s |
			BIPrettyPrinterContext new storeOn: s]"

	^ self contextClass basicNew
		useBasicCommentFormat: false;
		formatCommentWithStatements:true;
		indentString: '	';
		newLinesAfterTemporaries:0;
		retainBlankLinesBeforeComments:false;
		maxLineLength: 50
]

{ #category : #configurations }
BITemporariesExpressionTest >> retainBlankLinesBeforeComments [
	"Here we can control explicitly the configuration we want."

	" String streamContents: [:s |
			BIPrettyPrinterContext new storeOn: s]"
	^ self contextClass basicNew
		newLinesAfterTemporaries: 1;
		useBasicCommentFormat: false;
		formatCommentWithStatements:true;
		maxLineLength:50;
		indentString: '	';
		retainBlankLinesBeforeComments:true
]

{ #category : #tests }
BITemporariesExpressionTest >> testCommentBasicFormat [
	| source |
	configurationSelector := #basicCommentFormatConfiguration.
	source := self formatExpression: ' | d "    comment     " e |'.
	self assert: source equals: '| d	"    comment     " e |'
]

{ #category : #tests }
BITemporariesExpressionTest >> testCommentNotBasicFormat [
	| source |
	configurationSelector := #notBasicCommentFormatConfiguration.
	source := self formatExpression: ' | d "    comment     " e |'.
	self
		assert: source
		equals:
			'| d	"comment     
" e |'
]

{ #category : #tests }
BITemporariesExpressionTest >> testDontRetainBlankLinesBeforeComments [
	| source |
	configurationSelector := #dontRetainBlankLinesBeforeComments.
	source := self
		formatExpression:
			' | d 
	"comment"e |'.
	self
		assert: source
		equals:
			'| d"comment"
 e |'
]

{ #category : #tests }
BITemporariesExpressionTest >> testDontRetainBlankLinesBeforeComments2 [
	| source |
	configurationSelector := #dontRetainBlankLinesBeforeComments.
	source := self
		formatExpression:
			' | d 
	
	"comment"
	e |'.
	self
		assert: source
		equals:
			'| d"comment"
 e |'
]

{ #category : #tests }
BITemporariesExpressionTest >> testNewLineAfterTemporaries [
	| source |
	configurationSelector := #newLinesAfterTemporariesConfiguration.
	source := self formatExpression: ' |d e|'.
	self
		assert: source
		equals:
			'| d e |
'
]

{ #category : #tests }
BITemporariesExpressionTest >> testNoNewLineAfterTemporaries [
	| source |
	configurationSelector := #noNewLinesAfterTemporariesConfiguration.
	source := self formatExpression: ' |d e|'.
	self assert: source equals: '| d e |'
]

{ #category : #tests }
BITemporariesExpressionTest >> testRetainBlankLinesBeforeComments [
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
BITemporariesExpressionTest >> testRetainBlankLinesBeforeComments2 [
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
BITemporariesExpressionTest >> testRetainBlankLinesBeforeComments3 [
	| source |
	configurationSelector := #retainBlankLinesBeforeComments.
	source := self formatExpression: ' | d "comment" e |'.
	self
		assert: source
		equals:
			'| d	"comment" e |
'
]

{ #category : #tests }
BITemporariesExpressionTest >> testSpaceBetweenTemporaries [
	| source |
	configurationSelector := #noNewLinesAfterTemporariesConfiguration.
	source := self formatExpression: '|a|'.
	self assert: source equals: '| a |'
]

{ #category : #tests }
BITemporariesExpressionTest >> testSpaceBetweenTemporaries2 [
	| source |
	configurationSelector := #noNewLinesAfterTemporariesConfiguration.
	source := self formatExpression: '|a b|'.
	self assert: source equals: '| a b |'
]

{ #category : #tests }
BITemporariesExpressionTest >> testTabBetweenVarAndComment [
	| source |
	configurationSelector := #commentWithStatementsConfiguration.
	source := self formatExpression: ' | d "comment" |'.
	self assert: source equals: '| d	"comment" |'
]

{ #category : #tests }
BITemporariesExpressionTest >> testTwoLinesAfterTemporaries [
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
BITemporariesExpressionTest >> twoLinesAfterTemporariesConfiguration [
	"Here we can control explicitly the configuration we want."

	" String streamContents: [:s |
			BIPrettyPrinterContext new storeOn: s]"
		
	^ self contextClass basicNew
		newLinesAfterTemporaries: 2;
		useBasicCommentFormat: false;
		formatCommentWithStatements:false;
		indentString: '	';
		retainBlankLinesBeforeComments:false
]

Class {
	#name : #MicroDownParserTest,
	#superclass : #TestCase,
	#instVars : [
		'parser'
	],
	#category : #'Microdown-Tests'
}

{ #category : #running }
MicroDownParserTest >> parser [ 

	^ MicroDownParser new
]

{ #category : #running }
MicroDownParserTest >> setUp [
	"Hooks that subclasses may override to define the fixture of test."
	
	super setUp.
	parser := self parser
]

{ #category : #'tests - anchor' }
MicroDownParserTest >> testAnchorBlock [
	| source root annotated |
	source := parser anchorMarkup, 'label1'.
	root := parser parse: source.
	self assert: root children size equals: 1.
	annotated := root children first. 
	self assert: annotated label equals: 'label1'.
	

]

{ #category : #'tests - anchor' }
MicroDownParserTest >> testAnchorBlockFollowingEachOther [
	| source root annotated |
	source := parser anchorMarkup, 'label1', String cr.
	source := source, parser anchorMarkup, 'label2'.
	root := parser parse: source.
	self assert: root children size equals: 2.
	annotated := root children first. 
	self assert: annotated label equals: 'label1'.
	
	annotated := root children second. 
	self assert: annotated label equals: 'label2'.
	

]

{ #category : #'tests - anchor' }
MicroDownParserTest >> testAnchorMarkupInsideLine [
	| source root paragraph |
	source := 'this is not an anchor', parser anchorMarkup, 'label1'.
	root := parser parse: source.
	self assert: root children size equals: 1.
	paragraph := root children first. 
	self assert: (paragraph isKindOf: MicParagraphBlock).
	self assert: paragraph text equals: 'this is not an anchor', parser anchorMarkup, 'label1'.
	

]

{ #category : #'tests - annotated-paragraph' }
MicroDownParserTest >> testAnnotatedBlock [
	| source root annotated |
	source := parser annotatedParagraphMarkup , 'important this is an important paragraph on one line.'.
	root :=  parser parse: source.
	self assert: root children size equals: 1.
	annotated := root children first. 
	self assert: annotated label equals: 'important'.
	self assert: annotated body equals: 'this is an important paragraph on one line.'
	

]

{ #category : #'tests - annotated-paragraph' }
MicroDownParserTest >> testAnnotatedBlockOnMultipleLinesParserLogic [

	| source root annotated  line |
	source := (parser annotatedParagraphMarkup , 'important this is an 
important point because...!') readStream.
	
	"first the root block"
	root := parser parse: ''.
	self assert: parser current equals: root. 
	
	"second the annotated"
	annotated := parser handleLine: source nextLine.
	self assert: parser current equals: annotated.
	self assert: annotated parent equals: root.  
	
	self assert: annotated label equals: 'important'.
	self assert: annotated text equals: 'this is an'.
	
	"third the second line"
	line := source nextLine.
	self assert: (annotated canConsumeLine: line).
	annotated := parser handleLine: line.
	self assert: parser current equals: annotated.
	self assert: annotated text equals: 'this is an
important point because...!'.
	
]

{ #category : #'tests - annotated-paragraph' }
MicroDownParserTest >> testAnnotatedBlockOnTwoLines [
	| source root annotated |
	source := parser annotatedParagraphMarkup , 'important this is an important 
paragraph on two lines.'.
	root := parser parse: source.
	self assert: root children size equals: 1.
	annotated := root children first. 
	self assert: annotated label equals: 'important'.
	self assert: annotated text equals: 'this is an important
paragraph on two lines.'

]

{ #category : #'tests - annotated-paragraph' }
MicroDownParserTest >> testAnnotatedBlockWithListParserLogic [
	"The test does not test what we want. We want a listblock inside the annotatedBlock"
	|  source root annotated  line |
	self flag: #fixme.
	source := (parser annotatedParagraphMarkup , 'important this is a point
- one
* two ' ) readStream.

 	parser := self parser. 
	
	"first the root block"
	root := parser parse: ''.
	self assert: parser current equals: root. 
	
	"second the annotated"
	annotated := parser handleLine: source nextLine.
	self assert: parser current equals: annotated.
	self assert: annotated parent equals: root.  
	
	self assert: annotated label equals: 'important'.
	self assert: annotated text equals: 'this is a point'.
	
	"third the second line"
	line := source nextLine.
	
	self assert: (annotated canConsumeLine: line).
	annotated := parser handleLine: line.
	self assert: parser current equals: annotated.
	self assert: annotated text equals: 'this is a point
- one'.


]

{ #category : #'tests - comments' }
MicroDownParserTest >> testCommentOnMultipleLines [

	| source root comment |
	source := parser commentedLineMarkup , 'a comment', String cr.
	source := source, parser commentedLineMarkup , 'a second comment'.
	root :=  parser parse: source.
	self assert: root children size equals: 1.
	comment := root children first. 
	self assert: comment text equals: 'a comment
a second comment'
	
]

{ #category : #'tests - comments' }
MicroDownParserTest >> testCommentOnMultipleLinesWithASingleMarkup [

	| source root comment |
	source := parser commentedLineMarkup , 'a comment', String cr.
	source := source, 'a second comment'.
	root :=  parser parse: source.
	"we have two commented lines."
	
	self assert: root children size equals: 2.
	comment := root children first. 
	self assert: comment text equals: 'a comment'.
	self assert: root children second text equals: 'a second comment'
	
]

{ #category : #'tests - comments' }
MicroDownParserTest >> testCommentSimple [

	| source root comment |
	source := parser commentedLineMarkup , 'a comment'.
	root :=  parser parse: source.
	self assert: root children size equals: 1.
	comment := root children first. 
	self assert: comment text equals: 'a comment'
	
]

{ #category : #'tests - comments' }
MicroDownParserTest >> testCommentedLineMarkupInsideLine [

	| source root paragraph |
	source := 'this is not the start', parser commentedLineMarkup , 'a comment'.
	root :=  parser parse: source.
	self assert: root children size equals: 1.
	paragraph := root children first. 
	self assert: (paragraph isKindOf: MicParagraphBlock).
	self assert: paragraph text equals: 'this is not the start', parser commentedLineMarkup , 'a comment'
	
]

{ #category : #'tests - environment' }
MicroDownParserTest >> testEnvironment [

	| source root math environmentName |
	environmentName := 'card'.
	source := parser environmentOpeningBlockMarkup , environmentName, String cr, 
	 'blbababbab bababba', String cr,
	parser environmentClosingBlockMarkup, String cr.

	root :=  parser parse: source.
	self assert: root children size equals: 1.
	math := root children first. 
	self assert: (math isKindOf: MicEnvironmentBlock). 
	self assert: math name equals: environmentName.
	self assert: math body equals: 'blbababbab bababba'
	
]

{ #category : #tests }
MicroDownParserTest >> testEnvironmentCitationWithArguments [
	| source root environmentName contents child |
	environmentName := 'cite'.
	contents := 'Hello

Pharo'.

	source := parser environmentOpeningBlockMarkup, environmentName, parser argumentListStartDelimiter, 'a=1&b=17&c'
		, String cr , contents , String cr
		, parser environmentClosingBlockMarkup , String cr.
	root := parser parse: source.
	child := root children first.
	self assert: (child isKindOf: MicEnvironmentBlock).
	self assert: child name equals: environmentName.
	self assert: child body equals: contents.
	
	self assert: (child arguments at: 'a') equals: '1'.
	self assert: (child arguments at: 'b') equals: '17'.
	self assert: (child arguments at: 'c') equals: nil
]

{ #category : #'tests - environment' }
MicroDownParserTest >> testEnvironmentEatsNonClosedCodeMarkup [
	| source root math environmentName contents |
	environmentName := 'card'.
	contents := '```smalltalk
^ self foo'.
	source := parser environmentOpeningBlockMarkup , environmentName
		, String cr , contents , String cr
		, parser environmentClosingBlockMarkup , String cr.
	root := parser parse: source.
	self assert: root children size equals: 1.
	math := root children first.
	self assert: (math isKindOf: MicEnvironmentBlock).
	self assert: math name equals: environmentName.
	self assert: math body equals: contents
]

{ #category : #'tests - environment' }
MicroDownParserTest >> testEnvironmentWithArguments [
	| source root environment environmentName contents |
	environmentName := 'card'.
	contents := 'blbababbab
bababba

toto'.
	source := parser environmentOpeningBlockMarkup, environmentName, parser argumentListStartDelimiter, 'a=1&b=17&c'
		, String cr , contents , String cr
		, parser environmentClosingBlockMarkup , String cr.
	root := parser parse: source.
	self assert: root children size equals: 1.
	environment := root children first.
	
	self assert: (environment arguments at: 'a') equals: '1'.
	self assert: (environment arguments at: 'b') equals: '17'.
	self assert: (environment arguments at: 'c') equals: nil.
]

{ #category : #'tests - environment' }
MicroDownParserTest >> testEnvironmentWithMultipleLines [
	| source root math environmentName contents |
	environmentName := 'card'.
	contents := 'blbababbab
bababba

toto'.
	source := parser environmentOpeningBlockMarkup , environmentName
		, String cr , contents , String cr
		, parser environmentClosingBlockMarkup , String cr.
	root := parser parse: source.
	self assert: root children size equals: 1.
	math := root children first.
	self assert: (math isKindOf: MicEnvironmentBlock).
	self assert: math name equals: environmentName.
	self assert: math body equals: contents
]

{ #category : #'tests - error' }
MicroDownParserTest >> testError [

	self should: [ parser parse: '

| aaab | jkhjh |
| --- |
| bar |

'
	] raise: MicParsingError 
	
]

{ #category : #'tests - horizontal' }
MicroDownParserTest >> testHorizontalBlock [

	| source root horizontal |
	source := parser horizontalLineMarkup, 'ignored ignored ignore', String cr.
	root :=  parser parse: source.
	self assert: root children size equals: 1.
	horizontal := root children first. 
	self assert: (horizontal isKindOf: MicHorizontalLineBlock). 
	
]

{ #category : #'tests - horizontal' }
MicroDownParserTest >> testHorizontalBlockFollowedByList [

	| source root horizontal |
	source := parser horizontalLineMarkup, 'ignored ignored ignore', String cr, '- item1'.
	root :=  parser parse: source.
	self assert: root children size equals: 2.
	horizontal := root children first. 
	self assert: (horizontal isKindOf: MicHorizontalLineBlock). 
	self assert: (root children second isKindOf: MicUnorderedListBlock). 
	
]

{ #category : #'tests - horizontal line' }
MicroDownParserTest >> testHorizontalLineBlock [
	| source root horizontal |
	source := parser horizontalLineMarkup, 'label1'.
	root := parser parse: source.
	self assert: root children size equals: 1.
	horizontal := root children first. 
	self assert: (horizontal isKindOf: MicHorizontalLineBlock)
	
	




]

{ #category : #'tests - math' }
MicroDownParserTest >> testMathBlock [

	| source root math |
	source := parser mathOpeningBlockMarkup , String cr, 
	 '\mathcal{F} = \{ I \subseteq B | support(I) \geq minsup \}', String cr,
	parser mathClosingBlockMarkup, String cr.

	root :=  parser parse: source.
	self assert: root children size equals: 1.
	math := root children first. 
	self assert: (math isKindOf: MicMathBlock). 
	self assert: math body equals: '\mathcal{F} = \{ I \subseteq B | support(I) \geq minsup \}'
	
]

{ #category : #'tests - math' }
MicroDownParserTest >> testMathBlockOnMulitpleLines [

	| source root math |
	source := parser mathOpeningBlockMarkup , String cr, 
	 '\mathcal{F} = \{ I \subseteq B |', String cr, 'support(I) \geq minsup \}', String cr,
	parser mathClosingBlockMarkup, String cr.

	root :=  parser parse: source.
	self assert: root children size equals: 1.
	math := root children first. 
	self assert: (math isKindOf: MicMathBlock). 
	self assert: math body equals: '\mathcal{F} = \{ I \subseteq B |
support(I) \geq minsup \}'
	
]

{ #category : #'tests - math' }
MicroDownParserTest >> testMathBlockWithArguments [

	| source root math |
	source := parser mathOpeningBlockMarkup, 'label=equation&doNiceMath' , String cr, 
	 '\mathcal{F} = \{ I \subseteq B | support(I) \geq minsup \}', String cr,
	parser mathClosingBlockMarkup, String cr.

	root :=  parser parse: source.
	self assert: root children size equals: 1.
	math := root children first.
	self assert: (math arguments at: 'label') equals: 'equation'.
	self assert: (math arguments at: 'doNiceMath') equals: nil
]

{ #category : #'tests - metaData' }
MicroDownParserTest >> testMetaData [

	| source root math |
	source := parser metaDataOpeningBlockMarkup , String cr, 
	 '"author": "S Ducasse"', String cr,
	parser metaDataClosingBlockMarkup, String cr.

	root :=  parser parse: source.
	self assert: root children size equals: 1.
	math := root children first. 
	self assert: (math isKindOf: MicMetaDataBlock). 
	self assert: math body equals: '"author": "S Ducasse"'
	"note that the closing is not part of the body"
	
]

{ #category : #'tests - metaData' }
MicroDownParserTest >> testMetaData2 [

	| source root metadata |
	source := '{
"authors" : "S. Ducasse" ,
"title" : "Pharo by Example"
}'.

	root :=  parser parse: source.
	self assert: root children size equals: 1.
	metadata := root children first. 
	self assert: (metadata isKindOf: MicMetaDataBlock). 
	self assert: metadata body equals: '"authors" : "S. Ducasse" ,
"title" : "Pharo by Example"'
	"note that the closing is not part of the body"
	
]

{ #category : #'tests - metaData' }
MicroDownParserTest >> testMetaDataEatsOtherBlocks [

	| source root metadata |
	source := parser metaDataOpeningBlockMarkup , String cr, 
	 '"author": "S Ducasse"', String cr,
	parser headerMarkup, 'newheader here'.
	root :=  parser parse: source.
	self assert: root children size equals: 1.
	metadata := root children first. 
	self assert: (metadata isKindOf: MicMetaDataBlock). 
	self assert: metadata body equals: '"author": "S Ducasse"',String cr, parser headerMarkup, 'newheader here'.
	
]

{ #category : #'tests - metaData' }
MicroDownParserTest >> testMetaDataMultipleLines [

	| source root metadata |
	source := parser metaDataOpeningBlockMarkup , String cr, 
	 '"author": "S Ducasse",', String cr,
	 '"title": "Ze Great Book from Frenchies"', String cr,
	parser metaDataClosingBlockMarkup, String cr.

	root :=  parser parse: source.
	self assert: root children size equals: 1.
	metadata := root children first. 
	self assert: (metadata isKindOf: MicMetaDataBlock). 
	self assert: metadata body equals: '"author": "S Ducasse",
"title": "Ze Great Book from Frenchies"'
	"note that the closing is not part of the body"
	
]

{ #category : #'tests - horizontal line' }
MicroDownParserTest >> testParseLineWithOneCharacter [ 
	
	| document |
	document := parser parse: '
a'.
	self assert: document children size equals: 1. 
	self assert: document children first class equals: MicParagraphBlock. 
	
]

{ #category : #'tests - horizontal line' }
MicroDownParserTest >> testParseLineWithThreeCharacters [ 
	
	| document |
	document := parser parse: '
a`a'.
	self assert: document children size equals: 1. 
	self assert: document children first class equals: MicParagraphBlock. 
	
]

{ #category : #'tests - horizontal line' }
MicroDownParserTest >> testParseLineWithTwoCharacters [ 
	
	| document |
	document := parser parse: '
aa'.
	self assert: document children size equals: 1. 
	self assert: document children first class equals: MicParagraphBlock. 
	
]

{ #category : #'tests - breaking class comments' }
MicroDownParserTest >> testRBPatternVariableNode [
	| paragraph1 paragraph2 paragraph3 |
	
	paragraph1 := parser parse: 'The @ character can be combined with the name to match lists of items. If combined with the . character, then it will match a list of statement nodes (0 or more). If used without the . or # character, then it matches anything except for a list of statements. Combining the @ with the # is not supported.'.
	
	paragraph2 :=  parser parse: 'Adding another ` in the name will cause the search/replace to look for more matches inside the node that this node matched. This option should not be used for top-level expressions since that would cause infinite recursion (e.g., searching only for "``@anything").
'.
	paragraph3 := parser parse: 'Instance Variables:
	isAnything	<Boolean>	can we match any type of node
	isList	<Boolean>	can we match a list of items (@)
	isLiteral	<Boolean>	only match a literal node (#)
	isStatement	<Boolean>	only match statements (.)
	recurseInto	<Boolean>	search for more matches in the node we match (`)

'
]

{ #category : #'tests - table' }
MicroDownParserTest >> testSimpleTable [

	self skip.
	self flag: #fixMe.
	self shouldnt: [ parser parse: '

| aaab | jkhjh |
| --- |
| bar |

'
	] raise: MicParsingError 
	
]

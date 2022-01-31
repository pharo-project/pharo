"
The logic is to call the main parser so that the inline elements are in the blocks as after the closeMe phase of a Block (this is where the inline block are parsed). 
"
Class {
	#name : #MicrodownInlineParserTest,
	#superclass : #TestCase,
	#category : #'Microdown-Tests-Parser'
}

{ #category : #parsing }
MicrodownInlineParserTest >> parse: aString [
	"self new parse: 'foo_abbb_zz' "

	^ self parserClass parse: aString
]

{ #category : #parsing }
MicrodownInlineParserTest >> parseAndReturnElementsOfParagraphFor: aString [ 
 	"self new parse: 'foo_abbb_zz' "
	
 	^ (self parse: aString) children first children
 
]

{ #category : #parsing }
MicrodownInlineParserTest >> parser [
	^ self parserClass new 
]

{ #category : #parsing }
MicrodownInlineParserTest >> parserClass [ 
	^ MicroDownParser 
]

{ #category : #'tests - raw' }
MicrodownInlineParserTest >> rawTestingFor: aString [

 	| elements |
 	elements := self parseAndReturnElementsOfParagraphFor: 'abc{{', aString, '}}def'.
 	self assert: elements size equals: 3.
 	self assert: elements first substring equals: 'abc'.
 	self assert: elements second substring equals: aString.
 	self assert: elements second kind equals: 'raw'.
 	self assert: elements second openingDelimiter equals: '{{'.
 	self assert: elements second closingDelimiter equals: '}}'
]

{ #category : #'tests - format' }
MicrodownInlineParserTest >> testAPIToAccessChildrenOfNestedElements [

 	| elements bold |
 	elements := self parseAndReturnElementsOfParagraphFor: 'abc**_italicAndBold_ boldOnly**def'.
 	self assert: elements size equals: 3.

	"we can ask text to any element" 
	self assert: elements first substring equals: 'abc'.
	self assert: elements first text equals: 'abc'.
	
	bold := elements second.
	self assert: bold class equals: MicBoldFormatBlock.
	self assert: bold children size equals: 2.
	self assert: bold children first class equals: MicItalicFormatBlock.
	self assert: bold children first children first class equals: MicTextBlock.
	self assert: bold children second text equals: ' boldOnly'.
	self assert: bold children second class equals: MicTextBlock.
]

{ #category : #'tests - anchors' }
MicrodownInlineParserTest >> testAnchorReferenceWithoutClosure [
 	| elements |
 	elements := (self parseAndReturnElementsOfParagraphFor: 'abc*@anchorsdef').
 	self assert: elements size equals: 1.
 	self assert: elements first substring equals: 'abc*@anchorsdef'
]

{ #category : #'tests - anchors' }
MicrodownInlineParserTest >> testAnchorRefernce [
 	| elements |
 	elements := (self parseAndReturnElementsOfParagraphFor: 'abc*@anchors@*def').
 	self assert: elements size equals: 3.
 	self assert: elements first substring equals: 'abc'.
 	self assert: elements second substring equals: 'anchors'.
 	self assert: elements second kind equals: 'anchorReference'.
 	self assert: elements second openingDelimiter equals: '*@'.
 	self assert: elements second closingDelimiter equals: '@*'
]

{ #category : #'tests - annotations' }
MicrodownInlineParserTest >> testAnnotationBasic [
 	| elements annotation |
 	elements := self parseAndReturnElementsOfParagraphFor: 'needed ', MicAnnotationOpenerDelimiter markup ,'footnote | value=A foonote is annotation.', MicAnnotationCloserDelimiter markup.
	"pay attention an annotation is inside a paragraph (else this is an environment)"
	annotation := elements second.
	
 	self assert: annotation substring equals: 'footnote | value=A foonote is annotation.'.
	
 	self assert: annotation kind equals: 'annotation'.
 	self assert: annotation openingDelimiter equals: MicAnnotationOpenerDelimiter markup.
 	self assert: annotation closingDelimiter equals: MicAnnotationCloserDelimiter markup
 
]

{ #category : #'tests - annotations' }
MicrodownInlineParserTest >> testAnnotationPrintString [
 	
	| elements |
 	elements := self parseAndReturnElementsOfParagraphFor: 'abc',MicAnnotationOpenerDelimiter markup,'footnote | value=A foonote is annotation.&width=90',MicAnnotationCloserDelimiter markup,'def'.
 	self assert: elements size equals: 3.
 	self assert: elements second text equals: 'footnote | value=A foonote is annotation.&width=90'.
	self assert: elements second name equals: 'footnote'.
	self assert: elements second arguments keys equals: #('value' 'width'). 
]

{ #category : #'tests - annotations' }
MicrodownInlineParserTest >> testAnnotations [
 	| elements |
 	elements := self parseAndReturnElementsOfParagraphFor: 'abc',MicAnnotationOpenerDelimiter markup,'footnote | value=A foonote is annotation.',MicAnnotationCloserDelimiter markup,'def'.
 	self assert: elements size equals: 3.
 	self assert: elements first substring equals: 'abc'.
 	self assert: elements second substring equals: 'footnote | value=A foonote is annotation.'.
 	self assert: elements second kind equals: 'annotation'.
 	self assert: elements second openingDelimiter equals: MicAnnotationOpenerDelimiter markup.
 	self assert: elements second closingDelimiter equals: MicAnnotationCloserDelimiter markup
 
]

{ #category : #'tests - annotations' }
MicrodownInlineParserTest >> testAnnotationsWithoutClosure [
 	| elements |
 	elements := self parseAndReturnElementsOfParagraphFor: 'abc<?footnote | value=A foonote is anannotation.def'.
 	self assert: elements size equals: 1.
 	self assert: elements first substring equals: 'abc<?footnote | value=A foonote is anannotation.def'
 
]

{ #category : #'tests - format' }
MicrodownInlineParserTest >> testBold [
 	| elements |
 	elements := self parseAndReturnElementsOfParagraphFor: 'abc**bolds**def'.
 	self assert: elements size equals: 3.
 	self assert: elements first substring equals: 'abc'.
	self assert: elements first class equals: MicTextBlock.
	
 	self assert: elements second substring equals: 'bolds'.
	self assert: elements second class equals: MicBoldFormatBlock.
 	self assert: elements second kind equals: 'bold'.
	self assert: elements second children first class equals: MicTextBlock.
	
 	self assert: elements second openingDelimiter equals: '**'
 
]

{ #category : #'tests - format' }
MicrodownInlineParserTest >> testBoldsAndCorrectNestedItalics [

 	| elements bold italic |
 	elements := self parseAndReturnElementsOfParagraphFor: 'abc**_italicitalic_**def'.
 	self assert: elements size equals: 3.
 	self assert: elements first substring equals: 'abc'.
	bold := elements second.
	italic := bold children first.
	self assert: bold kind equals: 'bold'.
	self assert: italic kind equals: 'italic'.
		
 	self assert: italic substring equals: 'italicitalic'.
	self flag: #fixme. 
	"self assert: bold substring equals: 'italicitalic'"
 	
	

 
]

{ #category : #'tests - format' }
MicrodownInlineParserTest >> testBoldsAndNestedItalics [ 
 	| elements |
 	elements := (self parseAndReturnElementsOfParagraphFor: 'abc**_bolds_**def').
 	self assert: elements size equals: 3.
 	self assert: elements first substring equals: 'abc'.
 	self assert: elements second substring equals: '_bolds_'.
 	self assert: elements second kind equals: 'bold'.
 	self assert: elements second openingDelimiter equals: '**'
 
]

{ #category : #'tests - format' }
MicrodownInlineParserTest >> testBoldsAndNestedItalicsWithClosureAfterOpenItalics [

 	| elements |
 	elements := self parseAndReturnElementsOfParagraphFor: 'abc**_bolds**_def'.
 	self assert: elements size equals: 3.
 	self assert: elements first substring equals: 'abc'.
 	self assert: elements second substring equals: '_bolds'.
	self assert: elements third substring equals: '_def'.
 	self assert: elements second kind equals: 'bold'.
 	self assert: elements second openingDelimiter equals: '**'
 
]

{ #category : #'tests - format' }
MicrodownInlineParserTest >> testBoldsWithoutClosure [ 

 	| elements |
 	elements := self parseAndReturnElementsOfParagraphFor: 'abc**boldsdef'.
 	self assert: elements size equals: 1.
 	self assert: elements first substring equals: 'abc**boldsdef'
 
]

{ #category : #'tests - others' }
MicrodownInlineParserTest >> testEscape [

 	| elements |
 	elements := self parseAndReturnElementsOfParagraphFor: 'abc\_bolds_def'.
 	self assert: elements size equals: 1.
 	self assert: elements first substring equals: 'abc_bolds_def'.
 	self assert: elements first kind equals: 'basic'.
 
]

{ #category : #tests }
MicrodownInlineParserTest >> testEscapeAsLastCharacterDoesNotBreak [
	
	| elts  |
	elts  := self parseAndReturnElementsOfParagraphFor:  'ch\'.
	self assert: elts size equals: 1.
	self assert: elts first class  equals: MicTextBlock.
	self assert: elts first substring  equals: 'ch\'.
	
	elts  := self parseAndReturnElementsOfParagraphFor:  'ch\\'.
	self assert: elts size equals: 1.
	self assert: elts first class  equals: MicTextBlock.
	self assert: elts first substring  equals: 'ch\\'.
	
	elts  := self parseAndReturnElementsOfParagraphFor:  'ch\a'.
	self assert: elts size equals: 1.
	self assert: elts first class  equals: MicTextBlock.
	self assert: elts first substring  equals: 'cha'.
]

{ #category : #'tests - figure' }
MicrodownInlineParserTest >> testFigure [
 	| elements |
 	elements := self parseAndReturnElementsOfParagraphFor: 'abc![caption](http://www.pharo.org?parameters=value)def'.
 	self assert: elements size equals: 3.
 	self assert: elements first substring equals: 'abc'.
 	self assert: elements second caption equals: 'caption'.
 	self assert: elements second kind equals: 'figure'.
 	self assert: elements second openingDelimiter equals: '!['.
 	self assert: elements second closingDelimiter equals: ')'
]

{ #category : #'tests - figure' }
MicrodownInlineParserTest >> testFigureWithParameters [

	| elements figure |
	elements := self parseAndReturnElementsOfParagraphFor:
		            '![caption terminating with a .](http://www.pharo.org/pharoLogo.png?micAnchor=fig2&micWidth=80)'.
	self assert: elements size equals: 1.
	figure := elements first.
	self assert: figure caption equals: 'caption terminating with a .'.
	self assert: figure captionElements first text equals: 'caption terminating with a .'.
	self assert: figure arguments keys size equals: 2
]

{ #category : #'tests - format' }
MicrodownInlineParserTest >> testItalic [
 	| elements |
 	elements := self parseAndReturnElementsOfParagraphFor: 'abc_italics_def'.
 	self assert: elements size equals: 3.
 	self assert: elements first substring equals: 'abc'.
 	self assert: elements second substring equals: 'italics'.
 	self assert: elements second kind equals: 'italic'.
 	self assert: elements second openingDelimiter equals: '_'
]

{ #category : #'tests - format' }
MicrodownInlineParserTest >> testItalicsAndNestedBolds [
 	| elements |
 	elements := (self parseAndReturnElementsOfParagraphFor: 'abc_**italics**_def').
 	self assert: elements size equals: 3.
 	self assert: elements first substring equals: 'abc'.
 	self assert: elements second substring equals: '**italics**'.
 	self assert: elements second kind equals: 'italic'.
 	self assert: elements second openingDelimiter equals: '_'
]

{ #category : #'tests - format' }
MicrodownInlineParserTest >> testItalicsAndNestedBoldsWithClosureAfterOpenBolds [

 	| elements |
 	elements := self parseAndReturnElementsOfParagraphFor: 'abc_**italics_**def'.
 	self assert: elements size equals: 3.
 	self assert: elements first substring equals: 'abc'.
 	self assert: elements second substring equals: '**italics'.
	self assert: elements third substring equals: '**def'.
 	self assert: elements second kind equals: 'italic'.
 	self assert: elements second openingDelimiter equals: '_'
 
]

{ #category : #'tests - format' }
MicrodownInlineParserTest >> testItalicsWithoutClosure [

 	| elements |
 	elements := self parseAndReturnElementsOfParagraphFor: 'abc_italicsdef'.
 	self assert: elements size equals: 1.
 	self assert: elements first substring equals: 'abc_italicsdef'
]

{ #category : #'tests - link' }
MicrodownInlineParserTest >> testLink [
 	| elements |
 	elements := (self parseAndReturnElementsOfParagraphFor: 'abc[Pharo.Org](http://www.pharo.org)def').
 	self assert: elements size equals: 3.
 	self assert: elements first substring equals: 'abc'.
	
 	self assert: elements second caption equals: 'Pharo.Org'.
 	self assert: elements second kind equals: 'linkName'.
	self assert: elements second url printString equals: 'http://www.pharo.org'.
	
	
 	self assert: elements second openingDelimiter equals: '['.
 	self assert: elements second closingDelimiter equals: ')'
]

{ #category : #'tests - link' }
MicrodownInlineParserTest >> testLinkWithParameters [
 	| elements |
 	elements := (self parseAndReturnElementsOfParagraphFor: 'abc[GH Ducasse](https://github.com/Ducasse?tab=repositories)def').
 	self assert: elements size equals: 3.
 	self assert: elements first substring equals: 'abc'.
 	self assert: elements second substring equals: 'GH Ducasse'.
 	self assert: elements second kind equals: 'linkName'.
	
	self assert: elements second url printString equals: 'https://github.com/Ducasse?tab=repositories'.
	
 	self assert: elements second openingDelimiter equals: '['.
 	self assert: elements second closingDelimiter equals: ')'
]

{ #category : #'tests - others' }
MicrodownInlineParserTest >> testMath [
 	| elements |
 	elements := (self parseAndReturnElementsOfParagraphFor: 'abc$V_i = C_0 - C_3$def').
 	self assert: elements size equals: 3.
 	self assert: elements first substring equals: 'abc'.
 	self assert: elements second substring equals: 'V_i = C_0 - C_3'.
 	self assert: elements second kind equals: 'math'.
 	self assert: elements second openingDelimiter equals: '$'
]

{ #category : #'tests - format' }
MicrodownInlineParserTest >> testMonospace [

 	| elements |
 	elements := self parseAndReturnElementsOfParagraphFor: 'abc`monospaces`def'.
 	self assert: elements size equals: 3.
 	self assert: elements first substring equals: 'abc'.
 	self assert: elements second substring equals: 'monospaces'.
 	self assert: elements second kind equals: 'monospace'.
 	self assert: elements second openingDelimiter equals: '`'

]

{ #category : #'tests - format' }
MicrodownInlineParserTest >> testMonospacesWithoutClosure [
 	| elements |
 	elements := self parseAndReturnElementsOfParagraphFor: 'abc`monospacesdef'.
 	self assert: elements size equals: 1.
 	self assert: elements first substring equals: 'abc`monospacesdef'

]

{ #category : #'tests - raw' }
MicrodownInlineParserTest >> testRawWitoutClosure [

 	| elements |
 	elements := self parseAndReturnElementsOfParagraphFor: 'abc{{rawsdef'.
 	self assert: elements size equals: 1.
 	self assert: elements first substring equals: 'abc{{rawsdef'
]

{ #category : #'tests - raw' }
MicrodownInlineParserTest >> testRaws [
	self rawTestingFor: 'anything'
]

{ #category : #'tests - raw' }
MicrodownInlineParserTest >> testRawsDoesNotHandleWellCurly [
	| elements |
 	elements := self parseAndReturnElementsOfParagraphFor: 'abc{{', 'anything}', '}}def'.
 	self assert: elements size equals: 3.
 	self assert: elements first substring equals: 'abc'.
 	self assert: elements second substring equals: 'anything'.
	self assert: elements third substring equals: '}def'.
 	self assert: elements second kind equals: 'raw'.
 	self assert: elements second openingDelimiter equals: '{{'.
 	self assert: elements second closingDelimiter equals: '}}'
	
]

{ #category : #'tests to make green' }
MicrodownInlineParserTest >> testRawsDoesNotHandleWellEscape [
	self skip.
	self rawTestingFor: 'anything\*foo'.
	
]

{ #category : #'tests - raw' }
MicrodownInlineParserTest >> testRawsHandleWellCurlyIfNotLast [
	
	self rawTestingFor: 'anything}88'.

]

{ #category : #'tests - raw' }
MicrodownInlineParserTest >> testRawsWithBold [

 	self rawTestingFor: '**bold**'
]

{ #category : #'tests - raw' }
MicrodownInlineParserTest >> testRawsWithItalic [

 	self rawTestingFor: '_italic_'
]

{ #category : #'tests - raw' }
MicrodownInlineParserTest >> testRawsWithMonospace [
 	self rawTestingFor: '`monononono`'
]

{ #category : #'tests - raw' }
MicrodownInlineParserTest >> testRawsWithStrike [
 	self rawTestingFor: '~strikkkkk~'
]

{ #category : #'tests - format' }
MicrodownInlineParserTest >> testStrike [

 	| elements |
 	elements := self parseAndReturnElementsOfParagraphFor: 'abc~strikes~def'.
 	self assert: elements size equals: 3.
 	self assert: elements first substring equals: 'abc'.
 	self assert: elements second substring equals: 'strikes'.
 	self assert: elements second kind equals: 'strike'.
 	self assert: elements second openingDelimiter equals: '~'
]

{ #category : #'tests - format' }
MicrodownInlineParserTest >> testStrikesWithoutClosure [

 	| elements |
 	elements := self parseAndReturnElementsOfParagraphFor: 'abc~strikesdef'.
 	self assert: elements size equals: 1.
 	self assert: elements first substring equals: 'abc~strikesdef'

]

{ #category : #'tests - structure' }
MicrodownInlineParserTest >> testStructureOfAnchorReference [

 	| element |
 	element := (self parseAndReturnElementsOfParagraphFor: '  *@anchorReference@*') first.
	self assert: element text equals: 'anchorReference'.
	self assert: element children equals: #()
	
]

{ #category : #'tests - structure' }
MicrodownInlineParserTest >> testStructureOfAnnotation [

 	| element |
 	element := (self parseAndReturnElementsOfParagraphFor: MicAnnotationOpenerDelimiter markup , 'annotation' , MicAnnotationCloserDelimiter markup ) first.
	self assert: element text equals: 'annotation'.
	self assert: element name equals: 'annotation'.
	self assert: element children equals: #()
	
]

{ #category : #'tests - structure' }
MicrodownInlineParserTest >> testStructureOfAnnotationWithParameters [

 	| element |
 	element := (self parseAndReturnElementsOfParagraphFor: MicAnnotationOpenerDelimiter markup , 'inputFile|path=PharoTour.mic' , MicAnnotationCloserDelimiter markup ) first.
	self assert: element name equals: 'inputFile'.
	self assert: element children equals: #().
	self assert: element text equals: 'inputFile|path=PharoTour.mic'.
	self assert: (element arguments at: 'path') equals: 'PharoTour.mic'
	
]

{ #category : #'tests - structure' }
MicrodownInlineParserTest >> testStructureOfBoldNestedText [

 	| element |
 	element := (self parseAndReturnElementsOfParagraphFor: '**_abc_def**') first.
	self assert: element class equals: MicBoldFormatBlock.
	self assert: element substring equals: '_abc_def'.
	self assert: element children first substring equals: 'abc'.
	self assert: element children first class equals: MicItalicFormatBlock.
	self assert: element children second substring equals: 'def'.
	self assert: element children second class equals: MicTextBlock.
	self assert: element children first children first class equals: MicTextBlock.
	
	
]

{ #category : #'tests - structure' }
MicrodownInlineParserTest >> testStructureOfBoldSimpleText [

 	| element |
 	element := (self parseAndReturnElementsOfParagraphFor: '**abc def**') first.
	self assert: element class equals: MicBoldFormatBlock.
	self assert: element substring equals: 'abc def'.
	self assert: element children size equals: 1.
	self assert: element children first class equals: MicTextBlock.
	
]

{ #category : #'tests - structure' }
MicrodownInlineParserTest >> testStructureOfPlainText [

 	| element |
 	element := (self parseAndReturnElementsOfParagraphFor: 'abc') first.
	self assert: element text equals: 'abc'.
	self assert: element class equals: MicTextBlock.
	self assert: element children equals: #()
]

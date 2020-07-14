"
This test case uses the microdownSnippetFactory and test that the conversion to Pillar object trees is correct. 
This is why it is in this package

Microdown text -> Microdown trees -> Pillar trees

Te tests are just checking that object of the correct class is created. 
Future extensions should handle the details.
"
Class {
	#name : #MicSurfacicMicrodownToPillarTest,
	#superclass : #TestCase,
	#instVars : [
		'factory'
	],
	#category : #'Microdown-Pillar-Tests'
}

{ #category : #'tests - anchor' }
MicSurfacicMicrodownToPillarTest >> anchorClass [

	^ PRAnchor
]

{ #category : #'tests - codeblock' }
MicSurfacicMicrodownToPillarTest >> codeBlockClass [
	^ PRCodeblock
]

{ #category : #accessing }
MicSurfacicMicrodownToPillarTest >> factory [

	^ factory
]

{ #category : #accessing }
MicSurfacicMicrodownToPillarTest >> factory: aFactory [

	factory := aFactory
]

{ #category : #'tests - header' }
MicSurfacicMicrodownToPillarTest >> headerClass [
	^ PRHeader
]

{ #category : #'tests - list' }
MicSurfacicMicrodownToPillarTest >> orderedListClass [
	^ PROrderedList
]

{ #category : #accessing }
MicSurfacicMicrodownToPillarTest >> parse: sample createElementOfClass: aClass [

	| parser micDoc pillarDoc |
	parser := self parserClass new. 
	micDoc := parser parse: sample.
	pillarDoc := micDoc asPillar.
	self assert: pillarDoc children first class equals: aClass.
]

{ #category : #accessing }
MicSurfacicMicrodownToPillarTest >> parseCreateDocument: sample [ 

	| parser micDoc pillarDoc |
	parser := self parserClass new. 
	micDoc := parser parse: sample.
	pillarDoc := micDoc asPillar.
	self assert: pillarDoc class equals: PRDocument.
]

{ #category : #accessing }
MicSurfacicMicrodownToPillarTest >> parserClass [
	^ MicroDownParser   
]

{ #category : #'tests - document' }
MicSurfacicMicrodownToPillarTest >> rootBlockClass [

	^ PRDocument
]

{ #category : #running }
MicSurfacicMicrodownToPillarTest >> setUp [ 
	super setUp.
	self factory: MicMicroDownSnippetFactory new.
]

{ #category : #'tests - anchor' }
MicSurfacicMicrodownToPillarTest >> testAnchor [
	self parse: factory anchorSample createElementOfClass: self anchorClass
]

{ #category : #'tests - anchor' }
MicSurfacicMicrodownToPillarTest >> testAnchorAreTrimed [
	self parse: factory anchorAreTrimedSample createElementOfClass: self anchorClass
]

{ #category : #'tests - anchor' }
MicSurfacicMicrodownToPillarTest >> testAnchorWithNewLine [
	self parse: factory anchorWithNewLineSample createElementOfClass:  self anchorClass
]

{ #category : #'tests - anchor' }
MicSurfacicMicrodownToPillarTest >> testAnchorWithSpaceInside [
	self parse: factory anchorWithSpaceInsideSample createElementOfClass:  self anchorClass
]

{ #category : #'tests - format' }
MicSurfacicMicrodownToPillarTest >> testBoldFormat [
	self skip.
	self parse: factory boldFormatSample createElementOfClass: #format
]

{ #category : #'tests - format' }
MicSurfacicMicrodownToPillarTest >> testBoldFormatEmpty [
	self skip.
	self parse: factory boldFormatEmptySample createElementOfClass: #format
]

{ #category : #'tests - header' }
MicSurfacicMicrodownToPillarTest >> testHeaderEmpty [
	self parse: factory headerEmptySample createElementOfClass: self headerClass
]

{ #category : #'tests - header' }
MicSurfacicMicrodownToPillarTest >> testHeaderLevel1 [
	self parse: factory headerLevel1Sample createElementOfClass: self headerClass
]

{ #category : #'tests - header' }
MicSurfacicMicrodownToPillarTest >> testHeaderLevel2 [
	self parse: factory headerLevel2Sample createElementOfClass: self headerClass
]

{ #category : #'tests - header' }
MicSurfacicMicrodownToPillarTest >> testHeaderLevel3 [
	self parse: factory headerLevel3Sample createElementOfClass: self headerClass
]

{ #category : #'tests - header' }
MicSurfacicMicrodownToPillarTest >> testHeaderLevel4 [
	self parse: factory headerLevel4Sample createElementOfClass: self headerClass
]

{ #category : #'tests - header' }
MicSurfacicMicrodownToPillarTest >> testHeaderLevel5 [
	self parse: factory headerLevel5Sample createElementOfClass: self headerClass
]

{ #category : #'tests - header' }
MicSurfacicMicrodownToPillarTest >> testHeaderWithFormat [
	self parse: factory headerWithFormatSample createElementOfClass: self headerClass
]

{ #category : #'tests - header' }
MicSurfacicMicrodownToPillarTest >> testHeaderWithNewLine [
	self parse: factory headerWithNewLineSample createElementOfClass: self headerClass
]

{ #category : #'tests - format' }
MicSurfacicMicrodownToPillarTest >> testItalicFormat [
	self skip.
	self parse: factory italicFormatSample createElementOfClass: #format
]

{ #category : #'tests - format' }
MicSurfacicMicrodownToPillarTest >> testItalicFormatEmpty [
	self skip.
	self parse: factory italicFormatEmptySample createElementOfClass: #format
]

{ #category : #'tests - document' }
MicSurfacicMicrodownToPillarTest >> testLineEnd [
	self parseCreateDocument: factory lineEndSample 
]

{ #category : #'tests - document' }
MicSurfacicMicrodownToPillarTest >> testLineEndwithNewline [
	self parseCreateDocument: factory lineEndwithNewlineSample 
]

{ #category : #'tests - list' }
MicSurfacicMicrodownToPillarTest >> testListMulti [
	self parse: factory listMultiSample createElementOfClass: self unorderedListClass
]

{ #category : #'tests - list' }
MicSurfacicMicrodownToPillarTest >> testListWithComment [
	self skip.
	self parse: factory listWithCommentSample createElementOfClass: self orderedListClass 
]

{ #category : #'tests - format' }
MicSurfacicMicrodownToPillarTest >> testMonospaceFormat [
	self skip.
	self parse: factory monospaceFormatSample createElementOfClass: #format
]

{ #category : #'tests - format' }
MicSurfacicMicrodownToPillarTest >> testMonospaceFormatEmpty [
	self skip.
	self parse: factory monospaceFormatEmptySample createElementOfClass: #format
]

{ #category : #'tests - document' }
MicSurfacicMicrodownToPillarTest >> testNewline [
	self parseCreateDocument: factory newlineSample 
]

{ #category : #'tests - document' }
MicSurfacicMicrodownToPillarTest >> testNewlineCR [
	self parseCreateDocument: factory newlineCRSample 
]

{ #category : #'tests - document' }
MicSurfacicMicrodownToPillarTest >> testNewlineCRLF [
	self parseCreateDocument: factory newlineCRLFSample 
]

{ #category : #'tests - document' }
MicSurfacicMicrodownToPillarTest >> testNewlineLF [
	self parseCreateDocument: factory newlineLFSample 
]

{ #category : #'tests - list' }
MicSurfacicMicrodownToPillarTest >> testOrderedList [
	self parse: factory orderedListSample createElementOfClass: self orderedListClass
]

{ #category : #'tests - list' }
MicSurfacicMicrodownToPillarTest >> testOrderedListEmpty [
	self parse: factory orderedListEmptySample createElementOfClass: self orderedListClass
]

{ #category : #'tests - list' }
MicSurfacicMicrodownToPillarTest >> testOrderedListWithTwoItems [
	self
		parse: factory orderedListWithTwoItemsSample
		createElementOfClass: self orderedListClass
]

{ #category : #'tests - codeblock' }
MicSurfacicMicrodownToPillarTest >> testScript [
	self
		parse: factory scriptSample
		createElementOfClass: self codeBlockClass
]

{ #category : #'tests - codeblock internals' }
MicSurfacicMicrodownToPillarTest >> testScriptParameter [
	self skip.
	self
		parse: factory scriptParameterSample
		createElementOfClass: #parameterUntilNewline
]

{ #category : #'tests - codeblock internals' }
MicSurfacicMicrodownToPillarTest >> testScriptParameterEscape [
		self skip.
	self
		parse: factory scriptParameterEscapeSample
		createElementOfClass: #parameterUntilNewline
]

{ #category : #'tests - codeblock' }
MicSurfacicMicrodownToPillarTest >> testScriptParameterTrueByDefault [
	self 
		parse: factory scriptParameterTrueByDefaultSample 
		createElementOfClass: self codeBlockClass 
]

{ #category : #'tests - codeblock internals' }
MicSurfacicMicrodownToPillarTest >> testScriptParameterValue [
	self skip.
	self
		parse: factory scriptParameterValueSample
		createElementOfClass: #parameterValueUntilNewline
]

{ #category : #'tests - codeblock' }
MicSurfacicMicrodownToPillarTest >> testScriptParameterWithFloat [
	self parse: factory scriptParameterWithFloatSample createElementOfClass: self codeBlockClass 
]

{ #category : #'tests - codeblock' }
MicSurfacicMicrodownToPillarTest >> testScriptParameterWithNoValues [
	self parse: factory scriptParameterWithNoValuesSample createElementOfClass: self codeBlockClass 
]

{ #category : #'tests - codeblock internals' }
MicSurfacicMicrodownToPillarTest >> testScriptParameterWithSpacesBeforeAndAfter [
		self skip.
	self
		parse: factory scriptParameterWithSpacesBeforeAndAfterSample
		createElementOfClass: #scriptParameters
]

{ #category : #'tests - codeblock internals' }
MicSurfacicMicrodownToPillarTest >> testScriptParameterWithoutValue [
	self skip.
	self
		parse: factory scriptParameterWithoutValueSample
		createElementOfClass: #parameterUntilNewline
]

{ #category : #'tests - codeblock internals' }
MicSurfacicMicrodownToPillarTest >> testScriptParameters [
	self skip.
	self parse: factory scriptParametersSample createElementOfClass: #scriptParameters
]

{ #category : #'tests - codeblock internals' }
MicSurfacicMicrodownToPillarTest >> testScriptParametersEscape [
	self skip.
	self
		parse: factory scriptParametersEscapeSample
		createElementOfClass: #scriptParameters
]

{ #category : #'tests - codeblock internals' }
MicSurfacicMicrodownToPillarTest >> testScriptParametersMultiple [
	self skip.
	self
		parse: factory scriptParametersMultipleSample
		createElementOfClass: #scriptParameters
]

{ #category : #'tests - codeblock internals' }
MicSurfacicMicrodownToPillarTest >> testScriptTwoParametersNoValue [
	self skip.
	self
		parse: factory scriptTwoParametersNoValueSample
		createElementOfClass: #scriptParameters
]

{ #category : #'tests - codeblock' }
MicSurfacicMicrodownToPillarTest >> testScriptWithLanguage [
	self parse: factory scriptWithLanguageSample createElementOfClass: self codeBlockClass 
]

{ #category : #'tests - codeblock' }
MicSurfacicMicrodownToPillarTest >> testScriptWithNewLine [
	self parse: factory scriptWithNewLineSample createElementOfClass: self codeBlockClass 
]

{ #category : #'tests - codeblock' }
MicSurfacicMicrodownToPillarTest >> testScriptWithSpaceAtTheEnd [
	self parse: factory scriptWithSpaceAtTheEndSample createElementOfClass: self codeBlockClass 
]

{ #category : #'tests - codeblock' }
MicSurfacicMicrodownToPillarTest >> testScriptWithSpacedParameters [
	self parse: factory scriptWithSpacedParametersSample createElementOfClass: self codeBlockClass
]

{ #category : #'tests - format' }
MicSurfacicMicrodownToPillarTest >> testStrikethroughFormat [
	self skip.
	self parse: factory strikethroughFormatSample createElementOfClass: #format
]

{ #category : #'tests - format' }
MicSurfacicMicrodownToPillarTest >> testStrikethroughFormatEmpty [
	self skip.
	self parse: factory strikethroughFormatEmptySample createElementOfClass: #format
]

{ #category : #'tests - format' }
MicSurfacicMicrodownToPillarTest >> testSubscriptFormat [
	self skip.
	self parse: factory subscriptFormatSample createElementOfClass: #format
]

{ #category : #'tests - format' }
MicSurfacicMicrodownToPillarTest >> testSubscriptFormatEmpty [
	self skip.
	self parse: factory subscriptFormatEmptySample createElementOfClass: #format
]

{ #category : #'tests - format' }
MicSurfacicMicrodownToPillarTest >> testSuperscriptFormat [
	self skip.
	self parse: factory superscriptFormatSample createElementOfClass: #format
]

{ #category : #'tests - format' }
MicSurfacicMicrodownToPillarTest >> testSuperscriptFormatEmpty [
	self skip.
	self parse: factory superscriptFormatEmptySample createElementOfClass: #format
]

{ #category : #'tests - list' }
MicSurfacicMicrodownToPillarTest >> testUnorderedList [
	self parse: factory unorderedListSample createElementOfClass: self unorderedListClass 
]

{ #category : #'tests - list' }
MicSurfacicMicrodownToPillarTest >> testUnorderedListEmpty [
	self parse: factory unorderedListEmptySample createElementOfClass: self unorderedListClass 
]

{ #category : #'tests - list' }
MicSurfacicMicrodownToPillarTest >> testUnorderedListWithSubList [
	self
		parse: factory unorderedListWithSubListSample
		createElementOfClass: self unorderedListClass 
]

{ #category : #'tests - list' }
MicSurfacicMicrodownToPillarTest >> testUnorderedListWithTwoItems [
	self
		parse: factory unorderedListWithTwoItemsSample
		createElementOfClass: self unorderedListClass 
]

{ #category : #'tests - list' }
MicSurfacicMicrodownToPillarTest >> unorderedListClass [

	^ PRUnorderedList 
]

Class {
	#name : 'StylerCodeSnippetTest',
	#superclass : 'OCCodeSnippetTest',
	#category : 'Shout-Tests',
	#package : 'Shout-Tests'
}

{ #category : 'helpers' }
StylerCodeSnippetTest >> mockedStyledText [

	| text |
	text := snippet source asText.
	SHRBMockTextStyler new
		declared: #(#< #> #| #+ #- #-- #a #a: #abs #bar #e #foo #foo: #ifTrue:ifFalse: #max: #min: #sign #sign: #signal: #value value: );
		style: text ast: snippet doSemanticAnalysis.

	^ (1 to: text size)
		  collect: [ :index |
			  SHRBMockTextStyler patternStyleDictionary at:
				  (text attributesAt: index) anyOne name ]
		  as: String
]

{ #category : 'helpers' }
StylerCodeSnippetTest >> styledText [

	| text |
	text := snippet source asText.
	SHRBTextStyler new style: text ast: snippet doSemanticAnalysis.
	^ text
]

{ #category : 'tests' }
StylerCodeSnippetTest >> testStyler [
	"We should test more than that"

	| text |
	"Basic smoke test"
	text := self styledText.
	self assert: text isText.

	"Advanced mock test"
	text := self mockedStyledText.
	snippet styledPattern ifNotNil: [
		self assert: text equals: snippet styledPattern ]
]

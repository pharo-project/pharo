Extension { #name : 'OCCodeSnippet' }

{ #category : '*Shout-Tests' }
OCCodeSnippet >> mockedStyledText [

	| text |
	text := self source asText.
	SHRBMockTextStyler new
		declared: #(#< #> #| #+ #- #-- #a #a: #abs #bar #e #foo #foo: #ifTrue:ifFalse: #max: #min: #sign #sign: #signal: #value value: );
		style: text ast: self doSemanticAnalysis.

	^ (1 to: text size)
		  collect: [ :index |
			  SHRBMockTextStyler patternStyleDictionary at:
				  (text attributesAt: index) anyOne name ]
		  as: String
]

{ #category : '*Shout-Tests' }
OCCodeSnippet >> styledText [

	| text |
	text := self source asText.
	SHRBTextStyler new style: text ast: self doSemanticAnalysis.
	^ text
]

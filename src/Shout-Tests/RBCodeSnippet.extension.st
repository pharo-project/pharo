Extension { #name : #RBCodeSnippet }

{ #category : #'*Shout-Tests' }
RBCodeSnippet >> styledText [

	| text |
	text := self source asText.
	SHRBTextStyler new style: text ast: self doSemanticAnalysis.
	^ text
]

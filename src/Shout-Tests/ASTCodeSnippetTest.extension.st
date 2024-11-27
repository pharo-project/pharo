Extension { #name : 'ASTCodeSnippetTest' }

{ #category : '*Shout-Tests' }
ASTCodeSnippetTest >> testStyler [
	"We should test more than that"

	| text |
	"Basic smoke test"
	text := snippet styledText.
	self assert: text isText.

	"Advanced mock test"
	text := snippet mockedStyledText.
	snippet styledPattern ifNotNil: [
		self assert: text equals: snippet styledPattern ]
]

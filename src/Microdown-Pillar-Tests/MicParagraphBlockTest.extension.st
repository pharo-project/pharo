Extension { #name : #MicParagraphBlockTest }

{ #category : #'*Microdown-Pillar-Tests' }
MicParagraphBlockTest >> testSingleParagraphWithIndentedTextAsPillar [
	| source root expected pillar |
	source := 'This is
   a paragraph with spaces
	and one with tabs'.
	expected := 'This is
a paragraph with spaces
and one with tabs'.
	root := parser parse: source.
	pillar := root children first asPillar.
	self assert: pillar class equals: PRParagraph.
	self assert: pillar children first class equals: PRText.
	self assert: pillar children first text equals: expected
]

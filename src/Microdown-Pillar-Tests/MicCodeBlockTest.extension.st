Extension { #name : #MicCodeBlockTest }

{ #category : #'*Microdown-Pillar-Tests' }
MicCodeBlockTest >> testSingleCodeBlockTwoLinesAsPillar [
	| source root code textBody argument |
	textBody := 'my fancy code
is really cool'.
	argument := 'ArgumentGoesHere'.
	source := parser codeblockMarkup, argument, String cr, textBody, String cr, parser codeblockMarkup. 
	root := self parser parse: source.
	code := root children first asPillar.
	self
		assert: code text
		equals:
			'my fancy code
is really cool'.
	self assert: code language originalName equals: 'ArgumentGoesHere'
]

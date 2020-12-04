Extension { #name : #MicRootBlockTest }

{ #category : #'*Microdown-Pillar-Tests' }
MicRootBlockTest >> testRootAsPillar [
	| source root pillar |
	source := '####    Level four
## Level 2'.
	root := parser parse: source.
	pillar := root asPillar.
	self assert: pillar class equals: PRDocument.
	self assert: pillar children second class equals: PRHeader.
	self assert: pillar children first level equals: 4
]

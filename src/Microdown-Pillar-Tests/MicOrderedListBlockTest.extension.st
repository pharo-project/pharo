Extension { #name : #MicOrderedListBlockTest }

{ #category : #'*Microdown-Pillar-Tests' }
MicOrderedListBlockTest >> testSingleLevelList2asPillar [
	| source root list |
	source := '19. item 1
    expanded
7.  item 2'.
	root := parser parse: source.
	list := root children first asPillar.
	self assert: list class equals: PROrderedList .
	self assert: list children size equals: 2.
	self
		assert: list children first text
		equals:
			'item 1
expanded'.
	self assert: list counter equals: 19
]

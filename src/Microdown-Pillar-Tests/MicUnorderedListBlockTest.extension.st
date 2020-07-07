Extension { #name : #MicUnorderedListBlockTest }

{ #category : #'*Microdown-Pillar-Tests' }
MicUnorderedListBlockTest >> testSingleLevelList2asPillar [
	| source root list item |
	source := '- item 1
  expanded
- item 2'.
	root := parser parse: source.
	list := root children first asPillar.
	self assert: list class equals: PRUnorderedList.
	self assert: list children size equals: 2.
	item := list children first.
	self assert: item class equals: PRListItem.
	self
		assert: item children first text
		equals:
			'item 1
expanded'
]

{ #category : #'*Microdown-Pillar-Tests' }
MicUnorderedListBlockTest >> testTwoLevelList1AsPillar [
	| source root list secondItem subItem |
	source := '- item 1
  - sublevel
- item 2'.
	root := parser parse: source.
	list := root children first asPillar.
	secondItem := list children second.
	self assert: secondItem class equals: PRListItem.
	self assert: secondItem children first class equals: PRText.
	self assert: secondItem children first text equals: 'item 2'.
	subItem := list children first children second.
	self assert: subItem class equals: PRUnorderedList.
	self
		assert: subItem children first text
		equals: 'sublevel'	"item 1"	"unorded list"	"sub item"
]

examples
example
	"This test demonstrate a paginating morphTreeMorph in action."

	"self example."

	| treeMorph aWindow aModel |
	aModel := self itemsList: (1 to: 100) asArray.
	aWindow := aModel theme newWindowIn: World for: aModel title: 'Example'.
	treeMorph := aModel defaultTreeMorph.
	treeMorph pageSize: 30.
	treeMorph buildContents.
	aWindow addMorph: treeMorph fullFrame: LayoutFrame identity.
	aWindow open.
	aModel expandItemPath: {40}.
	self assert: treeMorph pager currentPage == 2
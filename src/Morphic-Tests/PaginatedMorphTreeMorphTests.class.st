Class {
	#name : #PaginatedMorphTreeMorphTests,
	#superclass : #TestCase,
	#category : 'Morphic-Tests-Widgets'
}

{ #category : #tests }
PaginatedMorphTreeMorphTests >> testPager [
	"This test demonstrate a paginating morphTreeMorph in action."

	| treeMorph aWindow aModel |
	aModel := PaginatedMorphTreeModel itemsList: (1 to: 100) asArray.
	aWindow := aModel theme newWindowIn: World for: aModel title: 'test'.
	treeMorph := aModel defaultTreeMorph.
	treeMorph pageSize: 30.
	treeMorph buildContents.
	aWindow addMorph: treeMorph fullFrame: LayoutFrame identity.
	aWindow open.
	aModel expandItemPath: { 40 }.
	self assert: treeMorph pager currentPage == 2.
	aWindow close
]

tests
testTypeOfSingleSelection
	| composer firstTreeMorph treeNodeMorph |
	composer := GLMCompositePresentation new.
	composer list.
	window := composer openOn: { {1} asSet }.
	firstTreeMorph := self find: MorphTreeMorph in: window.
	treeNodeMorph := firstTreeMorph scroller submorphs first.
	self send: firstTreeMorph mouseUpOnItem: treeNodeMorph.
	self assert: composer selection equals: {1} asSet
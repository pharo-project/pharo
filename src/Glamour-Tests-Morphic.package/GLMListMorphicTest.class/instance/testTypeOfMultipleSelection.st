tests
testTypeOfMultipleSelection
	| composer firstTreeMorph treeNodeMorph |
	composer := GLMCompositePresentation new.
	composer list beMultiple.
	window := composer openOn: { {1} asSet . {1} asOrderedCollection }.
	firstTreeMorph := self find: MorphTreeMorph in: window.
	treeNodeMorph := firstTreeMorph scroller submorphs first.
	self send: firstTreeMorph mouseUpOnItem: treeNodeMorph.
	self assert: composer selection asOrderedCollection equals: {{1} asSet} asOrderedCollection
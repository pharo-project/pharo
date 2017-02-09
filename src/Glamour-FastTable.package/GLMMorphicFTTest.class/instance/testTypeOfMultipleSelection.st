tests
testTypeOfMultipleSelection
	| composer firstTreeMorph treeNodeMorph |
	composer := GLMCompositePresentation new.
	composer fastList beMultiple.
	window := composer openOn: { {1} asSet . {1} asOrderedCollection }.
	firstTreeMorph := self find: FTTableMorph in: window.
	treeNodeMorph := firstTreeMorph selectionStrategy toggleRowIndex: 1.
	self assert: composer selection asOrderedCollection equals: {{1} asSet} asOrderedCollection
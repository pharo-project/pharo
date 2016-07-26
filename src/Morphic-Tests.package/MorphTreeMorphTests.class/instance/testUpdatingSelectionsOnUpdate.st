tests
testUpdatingSelectionsOnUpdate
	"Test that the selection does not refer to non-existing nodes after updates."

	| c w t |
	c := ClassTreeExample new.
	[ 
	w := c openOn: Collection.
	t := c dependents last.
	t expandAll.
	c
		selectItems:
			{Bag.
			CharacterSet}.
	c selection selectedNodes do: [ :e | self assert: (t selectedMorphList anySatisfy: [ :sm | sm complexContents = e ]) ].
	c rootClass: Bag.
	c updateList.
	c selection selectedNodes do: [ :e | self assert: (t selectedMorphList anySatisfy: [ :sm | sm complexContents = e ]) ] ]
		ensure: [ w close ]
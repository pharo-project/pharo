tests
testMaintainingSelectionsOnUpdate
	"Test that the selection is well maintained on list update."

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
	t selectedMorphList
		do: [ :e | 
			self
				assert:
					({Bag.
					CharacterSet} includes: e complexContents withoutListWrapper) ].
	c updateList.
	t selectedMorphList
		do: [ :e | 
			self
				assert:
					({Bag.
					CharacterSet} includes: e complexContents withoutListWrapper) ] ]
		ensure: [ w close ]
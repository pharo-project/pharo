tests
testLeakingSelectionsOnUpdate
	"Test that selected morphs are forgotten on update."

	| c w t |
	c := ClassTreeExample new.
	[ 
	w := c openOn: Collection.
	t := c dependents last.
	t expandAll.
	t selectAll.
	c updateList.
	t listManager selectedMorphList do: [ :each | self assert: (t allNodeMorphs includes: each) ] ]
		ensure: [ w close ]
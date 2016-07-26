tests
testUpdatingSelectionsOnUpdateWithRemoval
	"Test that the selection does not refer to non-existing nodes after updates, including if the update removes nodes."

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
	c rootClass: Bag.	"We change completely the list. CharacterSet is not anymore in the list after the update."
	c updateList.
	self assert: t selectedMorphList size = 1.	"We have only Bag selected now."
	self assert: t selectedMorphList first complexContents withoutListWrapper == Bag.
	c selection selectedNodes do: [ :e | self assert: (t selectedMorphList anySatisfy: [ :sm | sm complexContents = e ]) ] ]
		ensure: [ w close ]
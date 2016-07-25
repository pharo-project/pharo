tests
testLastClickedMorphSetUponUpdateList
	"Test that lastClickedMorph isn't set to nil after an update list."

	| c w t |
	c := ClassTreeExample new.
	[ 
	w := c openOn: Collection.
	t := c dependents last.
	t expandAll.
	c
		selectItems:
			{CharacterSet}.
	self assert: t listManager lastClickedMorph notNil.
	c rootClass: Bag.	"We change completely the list. CharacterSet is not anymore in the list after the update."
	c updateList.
	self assert: t listManager lastClickedMorph isNil.
	c selectItems: { Bag }.
	self assert: t listManager lastClickedMorph notNil ]
		ensure: [ w close ]
"
A MorphTreeMorphTests tests aspects of a MorphTreeMorph

"
Class {
	#name : #MorphTreeMorphTest,
	#superclass : #TestCase,
	#category : #'Morphic-Tests-Widgets'
}

{ #category : #tests }
MorphTreeMorphTest >> testLastClickedMorphSetUponUpdateList [
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
]

{ #category : #tests }
MorphTreeMorphTest >> testLeakingSelectionsOnUpdate [
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
]

{ #category : #tests }
MorphTreeMorphTest >> testMaintainingSelectionsOnUpdate [
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
]

{ #category : #tests }
MorphTreeMorphTest >> testUpdatingSelectionsOnUpdate [
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
]

{ #category : #tests }
MorphTreeMorphTest >> testUpdatingSelectionsOnUpdateWithRemoval [
	"Test that the selection does not refer to non-existing nodes after updates, including if the update removes nodes."

	| c w t |
	c := ClassTreeExample new.
	[ w := c openOn: Collection.
	t := c dependents last.
	t expandAll.
	c selectItems: {Bag . CharacterSet}.
	c selection selectedNodes do: [ :e | self assert: (t selectedMorphList anySatisfy: [ :sm | sm complexContents = e ]) ].
	c rootClass: Bag.	"We change completely the list. CharacterSet is not anymore in the list after the update."
	c updateList.
	self assert: t selectedMorphList size equals: 1.	"We have only Bag selected now."
	self assert: t selectedMorphList first complexContents withoutListWrapper identicalTo: Bag.
	c selection selectedNodes do: [ :e | self assert: (t selectedMorphList anySatisfy: [ :sm | sm complexContents = e ]) ] ]
		ensure: [ w close ]
]

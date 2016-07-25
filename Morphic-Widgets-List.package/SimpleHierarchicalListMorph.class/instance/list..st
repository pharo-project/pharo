initialization
list: aCollection

	| wereExpanded morphList |
	wereExpanded := self currentlyExpanded.
	scroller removeAllMorphs.
	(aCollection isNil or: [aCollection isEmpty]) ifTrue: [^ self selectedMorph: nil].
	morphList := OrderedCollection new.
	self 
		addMorphsTo: morphList
		from: aCollection 
		allowSorting: false
		withExpandedItems: wereExpanded
		atLevel: 0.
	self insertNewMorphs: morphList.
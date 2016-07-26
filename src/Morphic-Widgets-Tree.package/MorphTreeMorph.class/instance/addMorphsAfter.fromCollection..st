submorphs-add/remove
addMorphsAfter: parentMorph fromCollection: aCollection
	"Sent when expanding a node in order to add chilldren nodes after the expanded node"
	| priorMorph morphList  subs |
	priorMorph := nil.
	morphList := OrderedCollection new.
	
	"prepare the list of nodes to be added"
	aCollection do: [:item | 
		priorMorph := self indentingItemClass new 
			initWithContents: item 
			prior: priorMorph 
			forList: self
			indentLevel: parentMorph indentLevel + 1.
		morphList add: priorMorph.
		"Was this row expanded ? if true -> expand it
			again " 
			priorMorph isExpanded
				ifTrue: [priorMorph isExpanded: true.
					priorMorph
						addChildrenForList: self
						addingTo: morphList
						withExpandedItems: #()] ].
	
	"Set new child morphs index"
	1 to: morphList size do: [:i | | m | (m := morphList at: i) index: i + parentMorph index].
	"Add the new morph list in the scroller"
	scroller addAllMorphs: morphList after: parentMorph.
	"update next morphs index"
	subs := self allNodeMorphs.
	morphList last index to: subs size do: [:pos | (subs at: pos) index: pos].
	"set the new morphs widths according to columns width"
	self updateColumnMorphsWidth.
	morphList do: [ :e | e doLayoutIn: e layoutBounds ].
	^morphList
	

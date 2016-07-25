submorphs-add/remove
addMorphsTo: morphList from: aCollection withExpandedItems: expandedItems atLevel: newIndent 
	"Sent when building the list (by #buildContents), takes
	into accound old expanded items: they remain 
	expanded such that a list update don't change the list
	visual state"
	| priorMorph firstAddition |
	priorMorph := nil.
	firstAddition := nil.
	"also for the system progress bar"
	aCollection
		doWithIndex: [:item :idx | 
			priorMorph := self indentingItemClass new
						initWithContents: item
						prior: priorMorph
						forList: self
						indentLevel: newIndent.
			firstAddition
				ifNil: [firstAddition := priorMorph].
			morphList add: priorMorph.
			"Was this row expanded ? if true -> expand it
			again " 
			((item hasEquivalentIn: expandedItems)
					or: [priorMorph isExpanded])
				ifTrue: [priorMorph isExpanded: true.
					priorMorph
						addChildrenForList: self
						addingTo: morphList
						withExpandedItems: expandedItems]].
	^ firstAddition
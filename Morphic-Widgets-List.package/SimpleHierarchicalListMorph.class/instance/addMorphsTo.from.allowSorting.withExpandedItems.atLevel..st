private
addMorphsTo: morphList from: aCollection allowSorting: sortBoolean withExpandedItems: expandedItems atLevel: newIndent

	| priorMorph newCollection firstAddition |
	priorMorph := nil.
	newCollection := (sortBoolean and: [sortingSelector notNil]) ifTrue: [
		(aCollection asSortedCollection: [ :a :b | 
			(a perform: sortingSelector) <= (b perform: sortingSelector)]) asOrderedCollection
	] ifFalse: [
		aCollection
	].
	firstAddition := nil.
	newCollection do: [:item | 
		priorMorph := self indentingItemClass basicNew 
			initWithContents: item 
			prior: priorMorph 
			forList: self
			indentLevel: newIndent.
		firstAddition ifNil: [firstAddition := priorMorph].
		morphList add: priorMorph.
		((item hasEquivalentIn: expandedItems) or: [priorMorph isExpanded]) ifTrue: [
			priorMorph isExpanded: true.
			priorMorph 
				addChildrenForList: self 
				addingTo: morphList
				withExpandedItems: expandedItems.
		].
	].
	^firstAddition
	

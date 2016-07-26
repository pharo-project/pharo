private
selectSubmorphsOf: aMorph

	| newItems removals |
	newItems := aMorph submorphs select:
		[:m | (bounds containsRect: m fullBounds) 
					and: [m~~self
					and: [(m isKindOf: HaloMorph) not]]].
	otherSelection ifNil: [^ selectedItems := newItems].

	removals := newItems intersection: itemsAlreadySelected.
	otherSelection setSelectedItems: (itemsAlreadySelected copyWithoutAll: removals).
	selectedItems := (newItems copyWithoutAll: removals).

private
doneExtending

	otherSelection ifNotNil:
		[selectedItems := otherSelection selectedItems , selectedItems.
		otherSelection delete.
		self setOtherSelection: nil].
	self changed.
	self layoutChanged.
	super privateBounds:
		((Rectangle merging: (selectedItems collect: [:m | m fullBounds]))
			expandBy: 8).
	self changed.
	self addHalo.
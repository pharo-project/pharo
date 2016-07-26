adding
addARowCentered: aCollectionOfMorphs

	^(self addARow: aCollectionOfMorphs)
		hResizing: #shrinkWrap;
		wrapCentering: #center;
		cellPositioning: #leftCenter
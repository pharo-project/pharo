adding
addARowCentered: aCollectionOfMorphs cellInset: cellInsetInteger

	^(self addARow: aCollectionOfMorphs)
		hResizing: #shrinkWrap;
		wrapCentering: #center;
		cellPositioning: #leftCenter;
		cellInset: cellInsetInteger
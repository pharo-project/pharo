visual properties
inAColumn: aCollectionOfMorphs

	self setAsColumn
			color: Color transparent;
			vResizing: #shrinkWrap;
			hResizing: #shrinkWrap;
			layoutInset: 1;
			wrapCentering: #center;
			cellPositioning: #topCenter.
	aCollectionOfMorphs do: [ :each | self addMorphBack: each ].
	^ self 
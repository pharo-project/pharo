visual properties
inARightColumn: aCollectionOfMorphs 
	
	self setAsColumn 
				 vResizing: #shrinkWrap;
				 layoutInset: 1;
				 wrapCentering: #bottomRight;
				 cellPositioning: #topCenter.
	aCollectionOfMorphs
		do: [ :each | self addMorphBack: each ].
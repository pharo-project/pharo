visual properties
inARow: aCollectionOfMorphs 

	 self setAsRow 
				color: Color transparent;
				vResizing: #shrinkWrap;
				hResizing: #shrinkWrap;
				layoutInset: 1;
				wrapCentering: #center;
				cellPositioning: #leftCenter.
	aCollectionOfMorphs
		do: [ :each | self addMorphBack: each].

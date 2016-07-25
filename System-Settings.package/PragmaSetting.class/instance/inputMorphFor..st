user interface
inputMorphFor: aContainer 
	^ self inputWidget
		ifNotNil: [:iw | 
			( self theme newRowIn: World for: {iw})
				 clipSubmorphs: true;
				 hResizing: #shrinkWrap;
				 cellInset: 0;
				 yourself]
factory
buildWidget

	^ DiffMorph new  
		on: self ;
		from: self leftText to: self rightText contextClass: self contextClass;
		 hResizing:	#spaceFill;
		vResizing: #spaceFill;
		setBalloonText: self  help;
		showOptions: self showOptions;
		showOnlyDestination: self showOnlyDestination;
		yourself
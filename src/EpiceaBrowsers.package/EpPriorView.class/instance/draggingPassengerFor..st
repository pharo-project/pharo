private
draggingPassengerFor: items

	^ PanelMorph new
		changeTableLayout;
		vResizing: #shrinkWrap;
		hResizing: #shrinkWrap;
		listDirection: #topToBottom;
		cellPositioning: #topLeft;
		cellInset: 3;
		addAllMorphs: (self draggingPassengerMorphsFor: items);
		yourself
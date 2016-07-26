initialization
initializeBrowser

	super initializeBrowser.

	browser itemsModel
		dragEnabled: true;
		dropEnabled: false;
		transferBlock: [ :passenger :source | 
			| draggedItems |
			draggedItems := browser selectedItems.
			(TransferMorph 
				withPassenger: (self draggingPassengerFor: draggedItems)
				from: source)
				setProperty: #epiceaSourceView toValue: self;
				setProperty: #epiceaDraggedItems toValue: draggedItems;
				yourself ].
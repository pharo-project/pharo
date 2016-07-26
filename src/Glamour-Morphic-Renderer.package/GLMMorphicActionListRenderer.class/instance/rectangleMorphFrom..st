private
rectangleMorphFrom: anActionListPresentation
	^ BorderedMorph new
		borderWidth: 0;
		color: Color transparent;
		layoutPolicy: TableLayout new;
		cellPositioning: #topLeft;
		hResizing: #spaceFill;
		vResizing: #spaceFill;
		listDirection: (anActionListPresentation isHorizontal 
			ifTrue: [ #leftToRight ] 
			ifFalse: [ #topToBottom ] );
		listCentering: #topLeft;
		wrapCentering: #topLeft;
		cellInset: 5;
		yourself
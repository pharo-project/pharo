items creation
buildButtonsColumn

	| column |
	column := PanelMorph new.
	column 
		changeTableLayout;
		listDirection: #topToBottom.
		
		{
			self buildFullUpButton.
			self buildUpButton.
			self buildDownButton.
			self buildFullDownButton} reverse do: [:each | column addMorph: each ].
	
	column 
		vResizing: #spaceFill;
		width: 24;
		hResizing: #rigid.
	
	^ column
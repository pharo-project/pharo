event handling
scrollSelectionIntoView: evt 
	scrollPane ifNotNil: [ scrollPane scrollSelectionIntoView: evt ]	

as yet unclassified
defaultAction
	
	self openMenuIn: 
		[:labels :lines :caption | 
		UIManager default chooseFrom: labels lines: lines title: caption]
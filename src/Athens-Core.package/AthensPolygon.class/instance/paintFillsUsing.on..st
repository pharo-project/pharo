drawing
paintFillsUsing: aPaint on: anAthensCanvas 
	
	"This method is a part of rendering dispatch  Canvas->receiver->paint"
	
	^ aPaint fillPolygon: self on: anAthensCanvas
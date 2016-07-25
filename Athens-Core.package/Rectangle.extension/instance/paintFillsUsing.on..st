*Athens-Core
paintFillsUsing: aPaint on: anAthensCanvas 
	"This method is a part of rendering dispatch  Canvas->receiver->paint"
	
	^ aPaint athensFillRectangle: self on: anAthensCanvas
shortcut creation
positionShortcut: aMorph
	
	| area |
	
	area := World clearArea insetBy: 3. 
	
	aMorph 
	cellPositioning: position;
	fullBounds. 
	"the layout has to be computed before changing the position."
	
	aMorph perform: (position , ':') asSymbol with: (position value: area).
	
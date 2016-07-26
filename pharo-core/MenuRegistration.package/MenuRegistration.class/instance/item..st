sub item creating
item: aSymbol
	"set my name and my label with aSymbol (the label and the name as the same by default) and register myself as a child of my owner"
	^ (self builder newSubItem) name: aSymbol; label: aSymbol asString

updating
update: aSymbol
	"Handle tab changes."

	super update: aSymbol.
	aSymbol == #selectedIndex
		ifTrue: [self tabGroup page activate]
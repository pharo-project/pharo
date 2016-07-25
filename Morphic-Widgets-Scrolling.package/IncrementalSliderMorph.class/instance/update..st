updating
update: aSymbol
	"Update the value."
	
	super update: aSymbol.
	aSymbol = self getValueSelector
		ifTrue: [^self updateValue].
	aSymbol = self getEnabledSelector
		ifTrue: [^self updateEnabled]
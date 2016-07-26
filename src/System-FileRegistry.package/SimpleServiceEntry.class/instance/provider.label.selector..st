accessing
provider: anObject label: aString selector: aSymbol 
	"basic initialization message"
	provider := anObject.
	label := aString translated.
	selector := aSymbol.
	stateSelector := #none.
	description := ''
instance creation
provider: anObject label: aString selector: aSymbol description: anotherString buttonLabel: aLabel
	"Answer a new service object with the given initializations.  This variant allows a custom button label to be provided, in order to preserve precious horizontal real-estate in the button pane, while still allowing more descriptive wordings in the popup menu"

	^ self new provider: anObject label: aString selector: aSymbol description: anotherString; buttonLabel: aLabel; yourself
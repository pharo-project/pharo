intance creation
morph: aMorph state: aSymbol icon: aForm label: aString
	"Answer a new instance of the receiver with the given parameters."

	^self new
		morph: aMorph;
		state: aSymbol;
		icon: aForm;
		label: aString
updating
update: aSymbol
	"Update the receiver based on the given aspect.
	Override to not accept an #appendText for a text field
	since if broadcast by a model it will append to ALL
	text fields/editors."
	
	aSymbol == #appendEntry
		ifTrue: [^self].
	^super update: aSymbol
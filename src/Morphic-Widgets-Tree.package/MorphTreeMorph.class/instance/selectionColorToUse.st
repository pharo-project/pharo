selection
selectionColorToUse
	"Answer the colour to use for selected items."
	self enabled ifFalse: [ ^ self paneColor ].
	^ self valueOfProperty: #selectionColorToUse 
		ifAbsent: [ self theme selectionColor ].
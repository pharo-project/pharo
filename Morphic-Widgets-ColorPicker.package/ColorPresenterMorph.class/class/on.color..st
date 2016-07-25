instance creation
on: anObject color: getSel
	"Answer a new instance of the receiver on the given model using
	the given selectors as the interface."
	
	"(ColorPresenterMorph on: (BorderedMorph new) color: #color) openInWorld"
	
	^self new
		on: anObject 
		color: getSel
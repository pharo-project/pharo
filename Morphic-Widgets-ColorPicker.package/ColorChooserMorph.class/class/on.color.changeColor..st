instance creation
on: anObject color: getSel changeColor: setSel
	"Answer a new instance of the receiver on the given model using
	the given selectors as the interface."
	"(ColorChooserMorph on: (BorderedMorph new) color: #color changeColor: #color:) openInWorld"
	^self new
		on: anObject 
		color: getSel
		changeColor: setSel
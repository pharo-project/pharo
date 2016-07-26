as yet unclassified
model: anObject
	"Set the model and add the panel for it."
	
	super model: anObject.
	self paneMorphs copy do: [:p | p delete].
	self addMainPanel
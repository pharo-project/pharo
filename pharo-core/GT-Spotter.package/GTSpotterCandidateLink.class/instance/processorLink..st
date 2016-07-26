accessing
processorLink: anObject

	"we set rendering processor only if one is not set. it allows to have a collection of items,
	that have different rendering processors"
	
	processorLink ifNil: [ self renderingProcessor: anObject ].
	processorLink := anObject.
	
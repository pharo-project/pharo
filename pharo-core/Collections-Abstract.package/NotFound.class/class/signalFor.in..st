instance creation
signalFor: anObject in: aCollection
	"Create and signal a NotFound exception for anObject in aCollection."

	^ self new
		object: anObject;
		collection: aCollection;
		signal
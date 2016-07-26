instance creation
browse: aCollection
	"Special Version that sets the correct refreshing Block for Implentors Browser"
	^ self new
		messages: aCollection;
		open
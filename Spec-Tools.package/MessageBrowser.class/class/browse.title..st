instance creation
browse: aCollection title: aString
	"Special Version that sets the correct refreshing Block for Implentors Browser"
	^ self new
		messages: aCollection;
		title: aString;
		open
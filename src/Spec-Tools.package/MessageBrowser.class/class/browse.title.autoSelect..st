instance creation
browse: aCollection title: aString autoSelect: aSelectString
	"Special Version that sets the correct refreshing Block for Implentors Browser"
	^ self new
		messages: aCollection;
		title: aString;
		autoSelect: aSelectString;
		open
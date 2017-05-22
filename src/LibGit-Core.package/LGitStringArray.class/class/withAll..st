instance creation
withAll: stringCollection
	"Allocate an instance on the external heap and initialize it with
	<stringCollection>. Use autoRelease so the data will be freed when
	the instance is gc'ed."
	^ self externalNew
		count: stringCollection size;
		strings: stringCollection;
		autoRelease;
		yourself
instance creation
name: nameString email: emailString when: dateAndTime
	"Allocate an instance on the external heap and initialize it.
	Use autoRelease so the data will be freed when the instance is gc'ed."
	^ self externalNew
			name: nameString;
			email: emailString;
			dateAndTime: dateAndTime;
			autoRelease;
			yourself
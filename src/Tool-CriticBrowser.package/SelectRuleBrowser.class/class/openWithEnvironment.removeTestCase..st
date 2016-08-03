instance creation
openWithEnvironment: anEnvironment removeTestCase: aBoolean

	self new
		items: self items;
		environment: anEnvironment;
		removeTestCase: aBoolean;
		openWithSpec;
		updateTree
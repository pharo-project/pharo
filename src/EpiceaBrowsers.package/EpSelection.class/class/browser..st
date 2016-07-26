instance creation
browser: aBrowser

	^ self basicNew
		initializeWithBrowser: aBrowser;
		yourself
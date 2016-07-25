instance creation
for: aMethod in: aBrowser

	^ self new
		method: aMethod;
		browser: aBrowser;
		yourself
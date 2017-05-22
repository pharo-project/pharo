instance creation
name: aName url: anUrl
	^ (self url: anUrl)
		remoteName: aName;
		yourself
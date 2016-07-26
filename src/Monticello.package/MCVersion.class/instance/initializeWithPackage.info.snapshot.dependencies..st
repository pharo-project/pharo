initialization
initializeWithPackage: aPackage info: aVersionInfo snapshot: aSnapshot dependencies: aCollection
	package := aPackage.
	info := aVersionInfo.
	snapshot := aSnapshot.
	dependencies := aCollection.
	self addToCache.
printing
printOn: aStream
	| numberOfExtension |
	numberOfExtension := self extensionDependenciesSize.
	aStream
		nextPutAll: self packageName;
		nextPutAll: ' <-- Users : ';
		nextPutAll: self usersOfPackageSize asString;
		nextPutAll: ' package(s) | ';
		nextPutAll: 'Dependencies : ';
		nextPutAll: ((self dependenciesSize) - numberOfExtension) asString;
		nextPutAll: ' (+ ';
		nextPutAll: numberOfExtension asString;
		nextPutAll: ' extension dependencies ';
		nextPutAll: ')'.
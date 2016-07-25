printing
displayString
	| numberOfExtension |
	numberOfExtension := self extensionDependenciesSize.
	^ String streamContents: [ :stream | 
		stream
			nextPutAll: self packageName;
			nextPutAll: ' --> Dependent packages : ';
			nextPutAll: self dependentPackagesSize asString;
			nextPutAll: ' | Dependencies : ';
			nextPutAll: (self dependenciesSize - numberOfExtension) asString;
			nextPutAll: ' (+ ';
			nextPutAll: numberOfExtension asString;
			nextPutAll: ' extension dependencies)' ]
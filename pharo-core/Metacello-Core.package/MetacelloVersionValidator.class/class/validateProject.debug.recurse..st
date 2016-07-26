instance creation
validateProject: aMetacelloProject debug: debugList recurse: aBool
	^ ((self new)
		recurse: aBool;
		debug: debugList;
		configurationClass: aMetacelloProject configuration class;
		yourself) validateProject: aMetacelloProject
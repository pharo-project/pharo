instance creation
validateProject: aMetacelloProject version: versionString debug: debugList recurse: aBool

	^ ((self new)
		configurationClass: aMetacelloProject configuration class;
		debug: debugList;
		recurse: aBool;
		yourself) validateProject: aMetacelloProject version: versionString
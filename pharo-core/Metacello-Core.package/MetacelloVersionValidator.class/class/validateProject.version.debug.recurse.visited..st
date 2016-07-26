instance creation
validateProject: aMetacelloProject version: versionString debug: debugList recurse: aBool visited: visitied

	^(self new
		configurationClass: aMetacelloProject configuration class;
		debug: debugList;
		recurse: aBool;
		visited: visitied;
		yourself) validateProject: aMetacelloProject version: versionString
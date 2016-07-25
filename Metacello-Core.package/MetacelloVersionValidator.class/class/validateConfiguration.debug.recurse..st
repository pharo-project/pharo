instance creation
validateConfiguration: configurationClass debug: debugList recurse: aBool
	^ ((self new)
		configurationClass: configurationClass;
		debug: debugList;
		recurse: aBool;
		yourself) validate
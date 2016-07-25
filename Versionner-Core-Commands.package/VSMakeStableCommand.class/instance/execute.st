execute
execute
	"I expect that target is an MCVersion"
	(MetacelloToolBox configurationNamed: self configurationClass name)
		symbolicVersionMethod: #stable;
		addSymbolicSection: #common version: target versionString;
		commitMethod.

	
	
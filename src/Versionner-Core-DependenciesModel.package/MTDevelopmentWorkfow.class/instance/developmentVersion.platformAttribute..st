private
developmentVersion: devVersionString platformAttribute: platformAttribute
	"Set the #development symbolic version to the given version, using the given platformAttribute."
	
	(MetacelloToolBox configurationNamed: project name)
		symbolicVersionMethod: #development;
		addSymbolicSection: platformAttribute version: devVersionString;
		commitMethod.

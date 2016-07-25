scripts
stableVersion: stableVersionString for: configurationBasename platformAttribute: platformAttribute
	<apiDocumentation>
	"Set the #stable symbolic version to the given version, using the given platformAttribute."
	
	(MetacelloToolBox configurationNamed: configurationBasename)
		symbolicVersionMethod: #stable;
		addSymbolicSection: platformAttribute version: stableVersionString;
		commitMethod.

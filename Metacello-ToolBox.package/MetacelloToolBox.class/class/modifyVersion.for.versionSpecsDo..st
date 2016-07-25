scripts
modifyVersion: sourceVersionString for: configurationBasename versionSpecsDo: aBlock
	"Modify the version specifications for the given version"

	<apiDocumentation>
	(self configurationNamed: configurationBasename)
		modifyVersionMethodForVersion: sourceVersionString versionSpecsDo: aBlock;
		commitMethod
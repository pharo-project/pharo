scripts
createBaseline: baselineVersionString for: configurationBasename from: existingBaselineVersionString description: aString
	"Create a new baseline version based on the specification in an existing baseline"

	<apiDocumentation>
	^ self
		createBaseline: baselineVersionString
		for: configurationBasename
		from: existingBaselineVersionString
		description: aString
		versionSpecsDo: [ :attribute :versionSpec | true ]
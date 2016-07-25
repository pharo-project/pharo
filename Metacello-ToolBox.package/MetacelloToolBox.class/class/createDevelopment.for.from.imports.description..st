scripts
createDevelopment: developmentVersionString for: configurationBasename from: existingDevelopmentVersionString imports: imports description: aString
	"Create a new development version based on the specifications in an existing version, but override the imports for the new version. Modify the #development symbolic version to reference the new version."

	<apiDocumentation>
	| toolbox |
	toolbox := self
		createVersion: developmentVersionString
		for: configurationBasename
		from: existingDevelopmentVersionString
		description: aString.
	toolbox
		modifyVersionMethodForVersion: developmentVersionString;
		imports: imports;
		commitMethod.
	^ toolbox
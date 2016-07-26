*Versionner-Commit
createFixedVersion: targetVersionString for: configurationBasename from: sourceVersionString description: aString
	"Create a new version based on the development version.
	 I'm assuming Versionner style: development versions are baselines"
	<apiDocumentation>
	| toolbox sourceVersion |
	toolbox := self configurationNamed: configurationBasename.
	sourceVersion := toolbox project version: sourceVersionString.
	(targetVersionString indexOfSubCollection: 'baseline') > 0 
		ifTrue: [ self error: '' ].

	"create development version version from development version"
	self
		createFixedDevelopment: targetVersionString
		for: configurationBasename
		importFromBaseline: sourceVersion versionString
		description: aString.

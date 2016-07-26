development support
modifyDevelopmentVersionIn: aConfigurationClass imports: imports requiredProjects: projectList packages: packageList description: message
	"Modify #development version's baseline version based on the given structure
		imports - imports array
		projectList - collection of project base names (without ConfigurationOf prefix)
		packageList - collection of package base names
	"

	<apiDocumentation>
	| developmentVersion files |
	developmentVersion := aConfigurationClass project version: #development.
	files := OrderedCollection new.
	packageList do: [:packageName | files add: packageName -> packageName ].
	self
		modifyVersion: developmentVersion versionString
		section: #common
		for: aConfigurationClass name asString
		repository: nil
		requiredProjects: projectList
		packages: packageList
		dependencies: #()
		includes: #()
		files: files
		repositories: #()
		preLoadDoIts: #()
		postLoadDoIts: #()
		supplyingAnswers: #()
		groups: #().
	(self configurationNamed: aConfigurationClass name asString)
		modifyVersionMethodForVersion: developmentVersion versionString;
		imports: imports;
		commitMethod.
	self
		updateDevelopment: developmentVersion versionString
		for: aConfigurationClass name asString
		updateProjects: false 
		description: message
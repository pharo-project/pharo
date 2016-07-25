development support
createNewBaselineVersionIn: aConfigurationClass requiredProjects: projectList packages: packageList dependencies: dependencies  groups: groups
	"Create a new baseline version based upon #stable version's baseline, modify the baseline based on the provided structural additions:
		projectList - collection of project base names (without ConfigurationOf prefix)
		packageList - collection of package base names
		dependencies - collection of associations where key is name of package and value is collection of required packages and projects
		groups - collection of associations where key is the name of the group and value is the collection of group members
	"

	<apiDocumentation>
	| currentVersion existingBaselineVersionString baselineVersionNumber |
	currentVersion := aConfigurationClass project version: #development.
	existingBaselineVersionString := currentVersion importedVersions first.
	baselineVersionNumber := existingBaselineVersionString asMetacelloVersionNumber incrementMinorVersionNumber.
	self
		createBaseline: baselineVersionNumber versionString
		for: aConfigurationClass name asString
		from: existingBaselineVersionString
		description: ''.
	self
		modifyVersion: baselineVersionNumber versionString
		section: #common
		for: aConfigurationClass name asString
		repository: nil
		requiredProjects: projectList
		packages: packageList
		dependencies: dependencies
		includes: #()
		files: #()
		repositories: #()
		preLoadDoIts: #()
		postLoadDoIts: #()
		supplyingAnswers: #()
		groups: groups
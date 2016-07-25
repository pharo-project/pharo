development support
modifyBaselineVersionIn: aConfigurationClass repository: repositoryDescription requiredProjects: projectList packages: packageList dependencies: dependencies includes: includes files: files repositories: repositories preLoadDoIts: preLoadDoIts postLoadDoIts: postLoadDoIts supplyingAnswers: supplyingAnswers groups: groups
	"Modify #development version's baseline version based on the given structure
		repositoryDescription - metacello repository string
		projectList - collection of project base names (without ConfigurationOf prefix)
		packageList - collection of package base names
		dependencies - collection of associations where key is name of package and value is collection of required packages and projects
		includes - collection of associations where key is name of package and value is collection of included packages and projects
		files - collection of associations where key is name of package and value is name of file
		repositories - collection of associations where key is name of package and value is collection of repository descriptions
		preLoadDoIts - collection of associations where key is name of package and value is preload doit selector
		postLoadDoIts - collection of associations where key is name of package and value is postload doit selector
		supplyingAnswers - collection of associations where key is name of package and value is the supplyingAnswers array
		groups - collection of associations where key is the name of the group and value is the collection of group members
	"

	<apiDocumentation>
	| currentVersion existingBaselineVersionString |
	currentVersion := aConfigurationClass project version: #development.
	existingBaselineVersionString := currentVersion importedVersions first.
	self
		modifyVersion: existingBaselineVersionString
		section: #common
		for: aConfigurationClass name asString
		repository: repositoryDescription
		requiredProjects: projectList
		packages: packageList
		dependencies: dependencies
		includes: includes
		files: files
		repositories: repositories
		preLoadDoIts: preLoadDoIts
		postLoadDoIts: postLoadDoIts
		supplyingAnswers: supplyingAnswers
		groups: groups
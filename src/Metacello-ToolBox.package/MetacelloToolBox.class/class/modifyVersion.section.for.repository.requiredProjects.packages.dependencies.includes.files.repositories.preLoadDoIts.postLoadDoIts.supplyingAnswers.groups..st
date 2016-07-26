scripts
modifyVersion: sourceVersionString section: sectionAttributeOrPath for: configurationBasename repository: repositoryDescription requiredProjects: projectList packages: packageList dependencies: dependencies includes: includes files: files repositories: repositories preLoadDoIts: preLoadDoIts postLoadDoIts: postLoadDoIts supplyingAnswers: supplyingAnswers groups: groups
	"Modify a new version based on the specified project structure.
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
	| toolbox |
	(toolbox := self configurationNamed: configurationBasename)
		modifyVersionMethodForVersion: sourceVersionString versionSpecsDo: [:attribute :versionSpec | true ];
		modifySection: sectionAttributeOrPath asMetacelloAttributePath
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
			versionSpecsDo: [ :versionSpec | true ];
		commitMethod.
	^ toolbox
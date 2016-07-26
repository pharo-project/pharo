api-configuration
addSection: sectionAttributeOrPath repository: repositoryDescription requiredProjects: projectList packages: packageList repositories: repositories dependencies: dependecies groups: groups versionSpecsDo: aBlock
    "
		repositoryDescription - metacello repository string
		projectList - collection of project base names (without ConfigurationOf prefix)
		packageList - collection of package base names
		repositories - collection of associations where key is name of package and value is collection of repository descriptions
		dependencies - collection of associations where key is name of package and value is collection of required packages and projects
		groups - collection of associations where key is the name of the group and value is the collection of group members
	"

    self
        addSection: sectionAttributeOrPath
        repository: repositoryDescription
        requiredProjects: projectList
        packages: packageList
        dependencies: dependecies
        includes: #()
        files: #()
        repositories: repositories
        preLoadDoIts: #()
        postLoadDoIts: #()
        supplyingAnswers: #()
        groups: groups
        versionSpecsDo: aBlock
deprecated
createBaseline: baselineVersionString for: configurationBasename repository: repositoryDescription requiredProjects: projectList packages: packageList dependencies: dependencies groups: groups
    "Create a new baseline version based on the specified project structure.
		repositoryDescription - metacello repository string
		projectList - collection of project base names (without ConfigurationOf prefix)
		packageList - collection of package base names
		dependencies - collection of associations where key is name of package and value is collection of required packages and projects
		groups - collection of associations where key is the name of the group and value is the collection of group members
	"

    <apiDocumentation>
    self
        deprecated:
            'Use createBaseline:for: repository:requiredProjects:packages:repositories:dependencies:groups: instead'.
    ^ self
        createBaseline: baselineVersionString
        for: configurationBasename
        repository: repositoryDescription
        requiredProjects: projectList
        packages: packageList
        repositories: #()
        dependencies: dependencies
        groups: groups
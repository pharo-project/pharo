scripts
createBaseline: baselineVersionString for: configurationBasename repository: repositoryDescription requiredProjects: projectList packages: packageList repositories: repositories dependencies: dependencies groups: groups
    "Create a new baseline version based on the specified project structure.
		repositoryDescription - metacello repository string
		projectList - collection of project base names (without ConfigurationOf prefix)
		packageList - collection of package base names
		repositories - collection of associations where key is name of package and value is collection of repository descriptions
		dependencies - collection of associations where key is name of package and value is collection of required packages and projects
		groups - collection of associations where key is the name of the group and value is the collection of group members
	"

    <apiDocumentation>
    | toolbox |
    (toolbox := self configurationNamed: configurationBasename)
        createVersionMethod: 'baseline' , (self stripVersionStringForSelector: baselineVersionString) , ':'
            inCategory: 'baselines'
            forVersion: baselineVersionString;
        addSection: #'common'
            repository: repositoryDescription
            requiredProjects: projectList
            packages: packageList
            repositories: repositories
            dependencies: dependencies
            groups: groups
            versionSpecsDo: [ :versionSpec | versionSpec blessing: #'baseline' ];
        commitMethod.
    ^ toolbox
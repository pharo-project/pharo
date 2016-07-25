scripts
createBaselineOf: baselineBasename requiredProjects: projectList packages: packageList repositories: repositories dependencies: dependencies groups: groups
  "Create a new baseline in a BaselinOf based on the specified project structure.
		repositoryDescription - metacello repository string
		projectList - collection of project base names (without ConfigurationOf prefix)
		packageList - collection of package base names
		repositories - collection of associations where key is name of package and value is collection of repository descriptions
		dependencies - collection of associations where key is name of package and value is collection of required packages and projects
		groups - collection of associations where key is the name of the group and value is the collection of group members
	"

  <apiDocumentation>
  | toolbox |
  (toolbox := self baselineNamed: baselineBasename)
    createBaselineOfMethod: 'baseline:' inCategory: 'baseline';
    addBaselineOfSection: #'common'
      requiredProjects: projectList
      packages: packageList
      repositories: repositories
      dependencies: dependencies
      groups: groups
      versionSpecsDo: [ :versionSpec |  ];
    commitBaselineOfMethod.
  ^ toolbox
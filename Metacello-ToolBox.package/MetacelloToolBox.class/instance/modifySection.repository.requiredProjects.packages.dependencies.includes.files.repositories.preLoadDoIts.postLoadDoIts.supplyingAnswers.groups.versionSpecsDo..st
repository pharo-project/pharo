api-configuration
modifySection: sectionAttributeOrPath repository: repositoryDescription requiredProjects: newProjectList packages: newPackageList dependencies: dependecies includes: includes files: files repositories: repositories preLoadDoIts: preLoadDoIts postLoadDoIts: postLoadDoIts supplyingAnswers: supplyingAnswers groups: newGroups versionSpecsDo: aBlock
    "
		repositoryDescription - metacello repository string
		newProjectList - collection of project base names (without ConfigurationOf prefix)
		newPackageList - collection of package base names
		dependencies - collection of associations where key is name of package and value is collection of required packages and projects
		includes - collection of associations where key is name of package and value is collection of included packages and projects
		files - collection of associations where key is name of package and value is name of file
		repositories - collection of associations where key is name of package and value is collection of repository descriptions
		preLoadDoIts - collection of associations where key is name of package and value is preload doit selector
		postLoadDoIts - collection of associations where key is name of package and value is postload doit selector
		supplyingAnswers - collection of associations where key is name of package and value is the supplyingAnswers array
		newGroups - collection of associations where key is the name of the group and value is the collection of group members
	"

    self
        modifySection: sectionAttributeOrPath
        sectionIndex: 1
        repository: repositoryDescription
        requiredProjects: newProjectList
        packages: newPackageList
        dependencies: dependecies
        includes: includes
        files: files
        repositories: repositories
        preLoadDoIts: preLoadDoIts
        postLoadDoIts: postLoadDoIts
        supplyingAnswers: supplyingAnswers
        groups: newGroups
        versionSpecsDo: aBlock
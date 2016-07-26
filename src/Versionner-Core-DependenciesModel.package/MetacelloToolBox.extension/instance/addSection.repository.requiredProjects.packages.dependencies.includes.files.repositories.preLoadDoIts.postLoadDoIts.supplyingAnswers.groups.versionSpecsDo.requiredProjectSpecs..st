*Versionner-Core-DependenciesModel
addSection: sectionAttributeOrPath repository: repositoryDescription requiredProjects: projectList packages: packageList dependencies: dependecies includes: includes files: files repositories: repositories preLoadDoIts: preLoadDoIts postLoadDoIts: postLoadDoIts supplyingAnswers: supplyingAnswers groups: groups versionSpecsDo: aBlock requiredProjectSpecs: requiredProjectSpecs
    "
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
		requiredProjectSpecs - collection of already computed required project specs.

	"

    | versionSpec dependencyMap includesMap filesMap repositoriesMap preLoadDoItsMap postLoadDoItsMap supplyingAnswersMap |
    versionSpec := self createVersionSpec: self methodSpec versionString.
    repositoryDescription ~~ nil
        ifTrue: [ versionSpec repository: repositoryDescription ].
    dependencyMap := self buildMapFrom: dependecies for: packageList.
    includesMap := self buildMapFrom: includes for: packageList.
    filesMap := self buildMapFrom: files for: packageList.
    repositoriesMap := self buildMapFrom: repositories for: packageList , projectList.
    preLoadDoItsMap := self buildMapFrom: preLoadDoIts for: packageList.
    postLoadDoItsMap := self buildMapFrom: postLoadDoIts for: packageList.
    supplyingAnswersMap := self buildMapFrom: supplyingAnswers for: packageList.
    requiredProjectSpecs do: [ :projectSpec | versionSpec packages merge: projectSpec ].
    packageList
        do: [ :packageName | 
            | spec |
            spec := self createPackageSpec: packageName.
            dependencyMap at: packageName ifPresent: [ :dependencyList | spec requires: dependencyList ].
            includesMap at: packageName ifPresent: [ :includesList | spec includes: includesList ].
            filesMap at: packageName ifPresent: [ :file | spec file: file ].
            repositoriesMap
                at: packageName
                ifPresent: [ :repositoriesList | repositoriesList do: [ :repoString | spec repository: repoString ] ].
            preLoadDoItsMap at: packageName ifPresent: [ :preLoad | spec preLoadDoIt: preLoad ].
            postLoadDoItsMap at: packageName ifPresent: [ :postLoad | spec postLoadDoIt: postLoad ].
            supplyingAnswersMap at: packageName ifPresent: [ :answer | spec answers: answer ].
            versionSpec packages merge: spec ].
    groups
        do: [ :assoc | 
            | spec |
            spec := self createGroupSpec: assoc key.
            spec includes: assoc value.
            versionSpec packages merge: spec ].
    aBlock value: versionSpec.
    self methodSpec addMethodSection: sectionAttributeOrPath asMetacelloAttributePath versionSpec: versionSpec
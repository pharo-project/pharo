"
MetacelloToolbox implements a toolbox API for Metacello.

The toolbox methods on the class-side implement scripts for a number of commonly performed Metacello development tasks. The methods should be used in development scripts or GUI tools so that a common implementation is preserved across the toolset.
"
Class {
	#name : #MetacelloToolBox,
	#superclass : #Object,
	#instVars : [
		'project',
		'methodSpec'
	],
	#category : #'Metacello-ToolBox'
}

{ #category : #private }
MetacelloToolBox class >> appendDescription: aString to: versionSpec [
	aString isEmpty
		ifTrue: [ ^ self ].
	^ versionSpec description value isEmpty
		ifTrue: [ versionSpec description: aString ]
		ifFalse: [ 
			| strm |
			strm := WriteStream on: String new.
			strm
				nextPutAll: versionSpec description value;
				cr;
				nextPutAll: aString.
			versionSpec description: strm contents ]
]

{ #category : #utility }
MetacelloToolBox class >> baseNameOf: configurationClassName [
  "Return the baseName for the given configuration class name."

  <apiDocumentation>
  ^ (configurationClassName beginsWith: 'ConfigurationOf')
    ifTrue: [ 
      configurationClassName
        copyFrom: 'ConfigurationOf' size + 1
        to: configurationClassName size ]
    ifFalse: [ configurationClassName ]
]

{ #category : #utility }
MetacelloToolBox class >> baseNameOfConfiguration: aConfigurationClass [
	"Return the baseName for the given configuration class."

	<apiDocumentation>
	^self baseNameOf: aConfigurationClass name asString
]

{ #category : #utility }
MetacelloToolBox class >> baselineNameFrom: baseName [
  "Return the fully-qualified baseline class name."

  <apiDocumentation>
  ^ MetacelloScriptEngine baselineNameFrom: baseName
]

{ #category : #'instance creation' }
MetacelloToolBox class >> baselineNamed: baseName [
  ^ self new
    baselineNamed: baseName;
    yourself
]

{ #category : #utility }
MetacelloToolBox class >> checkForCriticalValidationIssues: configurationBasename [
	"Validate the named configuration and filter out all non-critical issues."
	<apiDocumentation>
	^ (MetacelloMCVersionValidator
		validateConfiguration: (Smalltalk at: (self configurationNameFrom: configurationBasename) asSymbol))
		select: [ :issue | issue isCritical ]
]

{ #category : #scripts }
MetacelloToolBox class >> compareVersion: fromVersionString for: configurationBasename to: toVersionString [
	"Return a MetacelloMCVersionDiffReport that whose fields 
		#additions
		#removals
		#modifications
	   refer to a dictionary whose keys are package names and whose 
	   values are a 2 element array. The first element is the mcz file 
	   name for the first version and the second element is the mcz file 
	   name for second version.
	There are also fields for the name of the configuration (#configuration), 
	the #from version string and the #to version string.
	
	The printString of the version report should provide all of the necessary info for 
	a developer."

	<apiDocumentation>
	| toolbox report |
	toolbox := self configurationNamed: configurationBasename.
	report := (toolbox project version: fromVersionString) difference: (toolbox project version: toVersionString).
	report 
		from: fromVersionString;
		to: toVersionString;
		configuration: configurationBasename.
	^ report
]

{ #category : #'development support' }
MetacelloToolBox class >> compareVersionsIn: aConfigurationClass [
	"Compare the current #stable version to current #development version"

	<apiDocumentation>
	| developmentVersion previousVersion |
	developmentVersion := aConfigurationClass project version: #development.
	previousVersion := aConfigurationClass project version: #stable.
	^self
		compareVersion: previousVersion versionString
		for: aConfigurationClass name asString
		to: developmentVersion versionString
	
]

{ #category : #'tool support' }
MetacelloToolBox class >> compiledMethodForVersion: aMetacelloVersion [

	| toolbox pragma |
	toolbox := MetacelloToolBox new project: aMetacelloVersion project.
	pragma := (toolbox constructor extractAllVersionPragmas at: aMetacelloVersion versionString ifAbsent: [ ^ nil ]) first.
	^pragma method
]

{ #category : #utility }
MetacelloToolBox class >> configurationClasses [
    "Return a set of the Metacello configuration classes that have been loaded into the image."

    <apiDocumentation>
    ^ MetacelloProjectRegistration configurationClasses
]

{ #category : #utility }
MetacelloToolBox class >> configurationNameFrom: baseName [
    "Return the fully-qualified configuration class name."

    <apiDocumentation>
    ^ MetacelloScriptEngine configurationNameFrom: baseName
]

{ #category : #'instance creation' }
MetacelloToolBox class >> configurationNamed: baseName [

	^self new 
		configurationNamed: baseName;
		yourself
]

{ #category : #validation }
MetacelloToolBox class >> copyConfiguration: aConfigurationClass to: metacelloRepositoryString [
	"Copy the current configuration mcz file to the given repository (i.e., http: //... or directory://, etc.)"

	<apiDocumentation>
	| project |
	project := aConfigurationClass project.
	project projectPackage
		copySpecTo:
			((project repositorySpec)
				description: metacelloRepositoryString;
				yourself)
]

{ #category : #scripts }
MetacelloToolBox class >> createBaseline: baselineVersionString for: configurationBasename from: existingBaselineVersionString description: aString [
	"Create a new baseline version based on the specification in an existing baseline"

	<apiDocumentation>
	^ self
		createBaseline: baselineVersionString
		for: configurationBasename
		from: existingBaselineVersionString
		description: aString
		versionSpecsDo: [ :attribute :versionSpec | true ]
]

{ #category : #scripts }
MetacelloToolBox class >> createBaseline: baselineVersionString for: configurationBasename from: existingBaselineVersionString description: aString versionSpecsDo: versionSpecBlock [
	"Create a new baseline version based on the specification in an existing baseline. Evaluate <versionSpecBlock> for each section."

	<apiDocumentation>
	| toolbox |
	(toolbox := self configurationNamed: configurationBasename)
		createVersionMethod: 'baseline' , (self stripVersionStringForSelector: baselineVersionString) , ':'
			inCategory: 'baselines'
			forVersion: baselineVersionString;
		addSectionsFrom: existingBaselineVersionString
			forBaseline: true
			updateProjects: false
			updatePackages: false
			versionSpecsDo: [ :attribute :versionSpec | 
				attribute == #common
					ifTrue: [ 
						aString notEmpty
							ifTrue: [ versionSpec description: aString ] ].
				versionSpecBlock value: attribute value: versionSpec ];
		commitMethod.
	^ toolbox
]

{ #category : #deprecated }
MetacelloToolBox class >> createBaseline: baselineVersionString for: configurationBasename repository: repositoryDescription requiredProjects: projectList packages: packageList dependencies: dependencies groups: groups [
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
]

{ #category : #scripts }
MetacelloToolBox class >> createBaseline: baselineVersionString for: configurationBasename repository: repositoryDescription requiredProjects: projectList packages: packageList dependencies: dependencies includes: includes files: files repositories: repositories preLoadDoIts: preLoadDoIts postLoadDoIts: postLoadDoIts supplyingAnswers: supplyingAnswers groups: groups [
	"Create a new baseline version based on the specified project structure.
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
		createVersionMethod: 'baseline' , (self stripVersionStringForSelector: baselineVersionString) , ':'
			inCategory: 'baselines'
			forVersion: baselineVersionString;
		addSection: #common
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
			versionSpecsDo: [ :versionSpec | versionSpec blessing: #baseline ];
		commitMethod.
	^ toolbox
]

{ #category : #scripts }
MetacelloToolBox class >> createBaseline: baselineVersionString for: configurationBasename repository: repositoryDescription requiredProjects: projectList packages: packageList repositories: repositories dependencies: dependencies groups: groups [
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
]

{ #category : #scripts }
MetacelloToolBox class >> createBaselineOf: baselineBasename requiredProjects: projectList packages: packageList repositories: repositories dependencies: dependencies groups: groups [
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
]

{ #category : #scripts }
MetacelloToolBox class >> createBranch: branchName for: configurationBasename description: commitMessage [
	"Create a new branch for the project and save the configuration to it's repository."
	
	<apiDocumentation>
	| toolbox criticalIssues |
	branchName isEmpty ifTrue: [ ^self saveConfigurationPackageFor: configurationBasename description: commitMessage ].
	(criticalIssues := self checkForCriticalValidationIssues: configurationBasename) isEmpty
		ifTrue: [ 
			toolbox := self configurationNamed: configurationBasename.
			toolbox project goferBranch: branchName project: commitMessage.
			^toolbox ].
	 self error: 'There are critical issues in configuration ' , configurationBasename printString, '. Configuration not saved'.
	^nil
]

{ #category : #scripts }
MetacelloToolBox class >> createDevelopment: developmentVersionString for: configurationBasename from: existingDevelopmentVersionString description: aString [
	"Create a new development version based on the specifications in an existing development version. Modify the #development symbolic version to reference the new version."

	<apiDocumentation>
	| toolbox |
	(toolbox := self configurationNamed: configurationBasename)
		createVersionMethod: 'version' , (self stripVersionStringForSelector: developmentVersionString) , ':'
			inCategory: 'versions'
			forVersion: developmentVersionString;
		addSectionsFrom: existingDevelopmentVersionString
			forBaseline: false
			updateProjects: true
			updatePackages: true
			versionSpecsDo: [ :attribute :versionSpec | 
				attribute == #common
					ifTrue: [ 
						"update the author, blessing, description and timeStamp"
						versionSpec
							author: MetacelloPlatform current authorName;
							description: aString;
							blessing: #development;
							timestamp: MetacelloPlatform current timestamp ].
				true ];
		commitMethod.
	toolbox
		symbolicVersionMethod: #development;
		addSymbolicSection: #common version: developmentVersionString;
		commitMethod.
	^ toolbox
]

{ #category : #scripts }
MetacelloToolBox class >> createDevelopment: developmentVersionString for: configurationBasename from: existingDevelopmentVersionString imports: imports description: aString [
	"Create a new development version based on the specifications in an existing version, but override the imports for the new version. Modify the #development symbolic version to reference the new version."

	<apiDocumentation>
	| toolbox |
	toolbox := self
		createVersion: developmentVersionString
		for: configurationBasename
		from: existingDevelopmentVersionString
		description: aString.
	toolbox
		modifyVersionMethodForVersion: developmentVersionString;
		imports: imports;
		commitMethod.
	^ toolbox
]

{ #category : #scripts }
MetacelloToolBox class >> createDevelopment: developmentVersionString for: configurationBasename importFromBaseline: baselineVersionString description: aString [
	"Create a new development version based on the specifications in an existing baseline version. Modify the #development symbolic version to reference the new version."

	<apiDocumentation>
	| toolbox |
	(toolbox := self configurationNamed: configurationBasename)
		createVersionMethod: 'version' , (self stripVersionStringForSelector: developmentVersionString) , ':'
			inCategory: 'versions'
			forVersion: developmentVersionString;
		importFrom: baselineVersionString
			updateProjects: true
			updatePackages: true
			versionSpecsDo: [ :attribute :versionSpec | 
				attribute == #common
					ifTrue: [ 
						"update the author, blessing, description and timeStamp"
						versionSpec
							author: MetacelloPlatform current authorName;
							blessing: #development;
							description: aString;
							timestamp: MetacelloPlatform current timestamp ].
				true ];
		commitMethod.
	toolbox
		symbolicVersionMethod: #development;
		addSymbolicSection: #common version: developmentVersionString;
		commitMethod.
	^ toolbox
]

{ #category : #'development support' }
MetacelloToolBox class >> createNewBaselineVersionIn: aConfigurationClass description: creationMessage [
	"Create a new baseline version based upon #stable version's baseline.
	 A new baseline should be created if new packages or projects have been added or package dependencies have changed."

	<apiDocumentation>
	| currentVersion existingBaselineVersionString baselineVersionNumber |
	currentVersion := aConfigurationClass project version: #stable.
	existingBaselineVersionString := currentVersion importedVersions first.
	baselineVersionNumber := existingBaselineVersionString asMetacelloVersionNumber incrementMinorVersionNumber.
	self
		createBaseline: baselineVersionNumber versionString
		for: aConfigurationClass name asString
		from: existingBaselineVersionString
		description: creationMessage
]

{ #category : #'development support' }
MetacelloToolBox class >> createNewBaselineVersionIn: aConfigurationClass description: creationMessage versionSpecsDo: versionSpecBlock [
	"Create a new baseline version based upon #stable version's baseline. Evaluate <versionSpecBlock> for each section.
	 A new baseline should be created if new packages or projects have been added or package dependencies have changed."

	<apiDocumentation>
	| currentVersion existingBaselineVersionString baselineVersionNumber |
	currentVersion := aConfigurationClass project version: #stable.
	existingBaselineVersionString := currentVersion importedVersions first.
	baselineVersionNumber := existingBaselineVersionString asMetacelloVersionNumber incrementMinorVersionNumber.
	self
		createBaseline: baselineVersionNumber versionString
		for: aConfigurationClass name asString
		from: existingBaselineVersionString
		description: creationMessage
		versionSpecsDo: versionSpecBlock
]

{ #category : #'development support' }
MetacelloToolBox class >> createNewBaselineVersionIn: aConfigurationClass requiredProjects: projectList packages: packageList dependencies: dependencies  groups: groups [
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
]

{ #category : #'development support' }
MetacelloToolBox class >> createNewDevelopmentVersionIn: aConfigurationClass description: creationMessage [
	"Create a new development version using the #stable version as model."

	<apiDocumentation>
	| currentVersion developmentVersion |
	currentVersion := aConfigurationClass project version: #stable.
	developmentVersion := currentVersion versionNumber copy incrementMinorVersionNumber.
	self
		createDevelopment: developmentVersion versionString
		for: aConfigurationClass name asString
		from: currentVersion versionString
		imports: currentVersion importedVersions
		description: creationMessage
]

{ #category : #scripts }
MetacelloToolBox class >> createVersion: targetVersionString for: configurationBasename from: sourceVersionString description: aString [
	"Create a new version based on the specifications in the given version. 
	 If the new version string contains 'baseline', then create a new baseline version.
	 If the given version is a baseline, then create a new version based on the given baseline.
	 If the given version is a non-baseline version, then create a new version based on the given version."

	<apiDocumentation>
	| toolbox sourceVersion |
	toolbox := self configurationNamed: configurationBasename.
	sourceVersion := toolbox project version: sourceVersionString.
	^ (targetVersionString indexOfSubCollection: 'baseline') > 0
		ifTrue: [ 
			"create baseline version from baseline version"
			sourceVersion blessing ~~ #baseline
				ifTrue: [ 
					self
						error:
							'Cannot create a baseline version ' , sourceVersionString printString , ' from a non-baseline version '
								, targetVersionString printString ].
			self
				createBaseline: targetVersionString
				for: configurationBasename
				from: sourceVersionString
				description: aString ]
		ifFalse: [ 
			sourceVersion blessing == #baseline
				ifTrue: [ 
					"create development version version from baseline version"
					self
						createDevelopment: targetVersionString
						for: configurationBasename
						importFromBaseline: sourceVersion versionString
						description: aString ]
				ifFalse: [ 
					"create development version version from development version"
					self
						createDevelopment: targetVersionString
						for: configurationBasename
						from: sourceVersion versionString
						description: aString ] ]
]

{ #category : #help }
MetacelloToolBox class >> defaultTopic: pageSelector for: aConfigurationClass [
	| stream helpTopicClass |
	(helpTopicClass := self helpTopicClass) ifNil: [^self error: 'Help system not installed'].
	stream := WriteStream on: String new.
	pageSelector == #helpInstallation
		ifTrue: [ 
			stream
				nextPutAll: 'To install this configuration, execute the following expression in a workspace:'; cr; tab.
			(aConfigurationClass project hasVersion: #stable)
				ifTrue: [ stream nextPutAll: '(', aConfigurationClass name asString, ' project version: #stable) load.' ]
				ifFalse: [ 
					(aConfigurationClass respondsTo: #load)
						ifTrue: [ stream nextPutAll: aConfigurationClass name asString , ' load.' ]
						ifFalse: [ stream nextPutAll: aConfigurationClass name asString, ' project latestVersion load.' ]].
			^ helpTopicClass perform: #title:contents: withArguments: { 'Installing'.  stream contents} ].
	pageSelector == #helpIntro
		ifTrue: [ 
			stream
				nextPutAll: 'I am a Metacello configuration for the ', (MetacelloToolBox baseNameOfConfiguration: aConfigurationClass), ' project.'.
			^ helpTopicClass perform: #title:contents: withArguments: { 'Intro' . stream contents} ].
	pageSelector == #helpGettingStarted
		ifTrue: [ 
			stream
				nextPutAll: 'I guess you''re on your own:).'.
			^ helpTopicClass perform: #title:contents: withArguments: { 'Getting Started' .stream contents} ].

]

{ #category : #validation }
MetacelloToolBox class >> descriptionForValidationReasonCode: reasonCode [
	"Description of validation reasonCode"

	<apiDocumentation>
	^MetacelloMCVersionValidator descriptionForReasonCode: reasonCode
]

{ #category : #help }
MetacelloToolBox class >> helpTopicClass [

	^self environment at: #HelpTopic ifAbsent: []
]

{ #category : #help }
MetacelloToolBox class >> helpTopicFor: aConfigurationClass [
	| topic pages page helpTopicClass |
	(helpTopicClass := self helpTopicClass) ifNil: [^self error: 'Help system not installed'].
	topic := helpTopicClass named: (self baseNameOfConfiguration: aConfigurationClass).
	pages := (aConfigurationClass respondsTo: #helpPages)
		ifTrue: [ aConfigurationClass perform: #helpPages ]
		ifFalse: [ #(#helpIntro #helpInstallation #helpGettingStarted) ].
	pages
		do: [ :pageSelectorOrClass | 
			page := (Smalltalk hasClassNamed: pageSelectorOrClass asString)
				ifTrue: [ (Smalltalk classNamed: pageSelectorOrClass asString) perform: #asHelpTopic ]
				ifFalse: [ 
					[ aConfigurationClass perform: pageSelectorOrClass ]
						on: MessageNotUnderstood
						do: [ :ex | self defaultTopic: pageSelectorOrClass for: aConfigurationClass ] ].
			topic perform: #addSubtopic: with: page ].
	^ topic
]

{ #category : #'development support' }
MetacelloToolBox class >> modifyBaselineVersionIn: aConfigurationClass repository: repositoryDescription requiredProjects: projectList packages: packageList dependencies: dependencies includes: includes files: files repositories: repositories preLoadDoIts: preLoadDoIts postLoadDoIts: postLoadDoIts supplyingAnswers: supplyingAnswers groups: groups [
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
]

{ #category : #'development support' }
MetacelloToolBox class >> modifyDevelopmentVersionIn: aConfigurationClass imports: imports requiredProjects: projectList packages: packageList description: message [
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
]

{ #category : #scripts }
MetacelloToolBox class >> modifySymbolicVersion: symbolicVersionSymbol for: configurationBasename attribute: attribute to: versionString [
	"Set the value of the symbolic version for the given attribute to the given version string"

	<apiDocumentation>
	| toolbox |
	(toolbox := self configurationNamed: configurationBasename)
		modifySymbolicVersionMethodFor: symbolicVersionSymbol;
		addSymbolicSection: attribute version: versionString;
		commitMethod.
	^ toolbox
]

{ #category : #scripts }
MetacelloToolBox class >> modifyVersion: sourceVersionString for: configurationBasename projectDo: projectBlock packageDo: packageBlock groupDo: groupBlock [
	"Modify the project, package and/or group specifications for the given version"

	<apiDocumentation>
	self
		modifyVersion: sourceVersionString
		for: configurationBasename
		versionSpecsDo: [ :attribute :versionSpec | 
			versionSpec
				specListProjectDo: [ :spec | projectBlock value: attribute value: versionSpec value: spec ]
				packageDo: [ :spec | packageBlock value: attribute value: versionSpec value: spec ]
				groupDo: [ :spec | groupBlock value: attribute value: versionSpec value: spec ].
			true ]
]

{ #category : #scripts }
MetacelloToolBox class >> modifyVersion: sourceVersionString for: configurationBasename versionSpecsDo: aBlock [
	"Modify the version specifications for the given version"

	<apiDocumentation>
	(self configurationNamed: configurationBasename)
		modifyVersionMethodForVersion: sourceVersionString versionSpecsDo: aBlock;
		commitMethod
]

{ #category : #scripts }
MetacelloToolBox class >> modifyVersion: sourceVersionString section: sectionAttributeOrPath for: configurationBasename repository: repositoryDescription requiredProjects: projectList packages: packageList dependencies: dependencies includes: includes files: files repositories: repositories preLoadDoIts: preLoadDoIts postLoadDoIts: postLoadDoIts supplyingAnswers: supplyingAnswers groups: groups [
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
]

{ #category : #utility }
MetacelloToolBox class >> platformAttributes [
	"Answer the list of platform attributes for the current platform"
	<apiDocumentation>
	^MetacelloPlatform current defaultPlatformAttributes
]

{ #category : #'development support' }
MetacelloToolBox class >> releaseDevelopmentVersionIn: aConfigurationClass description: commitMessage [
	"Release #development version: set version blessing to #release, update the #development and #stable symbolic version methods and save the configuration."

	<apiDocumentation>
	| version |
	version := aConfigurationClass project version: #development.
	self
		releaseVersion: version versionString for: aConfigurationClass name asString;
		saveConfigurationPackageFor: aConfigurationClass name asString description: commitMessage
]

{ #category : #scripts }
MetacelloToolBox class >> releaseEarlyAccessVersion: earlyAccessVersionString for: configurationBasename from: developmentVersionString [
	"Update the #development and #earlyAccess symbolic versions."

	<apiDocumentation>
	| toolbox message previousEarlyAccessVersion previousEarlyAccessSeparator index earlyAccessDescription |
	self flag: 'Make this more general purpose'.
	toolbox := self configurationNamed: configurationBasename.
	message := (toolbox project version: developmentVersionString) description.
	previousEarlyAccessVersion := (MetacelloVersionNumber fromString: earlyAccessVersionString) decrementMinorVersionNumber
		asString.
	previousEarlyAccessSeparator := '---' , previousEarlyAccessVersion , '---'.
	(index := message indexOfSubCollection: previousEarlyAccessSeparator) > 0
		ifTrue: [ 
			index + previousEarlyAccessSeparator size + 1 <= message size
				ifTrue: [ earlyAccessDescription := message copyFrom: index + previousEarlyAccessSeparator size + 1 to: message size ] ].
	toolbox
		createVersionMethod: 'version' , (self stripVersionStringForSelector: earlyAccessVersionString) , ':'
			inCategory: 'versions'
			forVersion: earlyAccessVersionString;
		addSectionsFrom: developmentVersionString
			forBaseline: false
			updateProjects: true
			updatePackages: true
			versionSpecsDo: [ :attribute :versionSpec | 
				attribute == #common
					ifTrue: [ 
						"update the author, blessing, description and timeStamp"
						versionSpec
							author: MetacelloPlatform current authorName;
							description: earlyAccessDescription;
							blessing: #development;
							timestamp: MetacelloPlatform current timestamp ].
				true ];
		commitMethod.
	toolbox
		symbolicVersionMethod: #earlyAccess;
		addSymbolicSection: #common version: earlyAccessVersionString;
		commitMethod.
	toolbox
		modifyVersionMethodForVersion: developmentVersionString
			versionSpecsDo: [ :attribute :versionSpec | 
					attribute == #common
						ifTrue: [ 
							"update the author, description and timeStamp"
							versionSpec
								author: MetacelloPlatform current authorName;
								timestamp: MetacelloPlatform current timestamp.
							self appendDescription: '---' , earlyAccessVersionString , '---' to: versionSpec ].
					true ];
		commitMethod.
	^ toolbox
]

{ #category : #scripts }
MetacelloToolBox class >> releaseVersion: developmentVersionString for: configurationBasename [
	"Set the #common blessing to #release and set the #common #stable symbolic version to the given version."

	<apiDocumentation>
	^ self
		releaseVersion: developmentVersionString
		for: configurationBasename
		blessingAttribute: #common
		platformAttribute: #common
]

{ #category : #scripts }
MetacelloToolBox class >> releaseVersion: developmentVersionString for: configurationBasename blessingAttribute: attribute platformAttribute: platformAttribute [
	<apiDocumentation>
	"Set the blessing to #release and set the #stable symbolic version to the given version, using the given attributes for each."
	
	| toolbox |
	(toolbox := self configurationNamed: configurationBasename)
		modifyVersionMethodForVersion: developmentVersionString
			versionSpecsDo: [ :attr :versionSpec | 
					attr == attribute
						ifTrue: [ versionSpec blessing: #release ].
					true ];
		commitMethod;
		symbolicVersionMethod: #development;
		addSymbolicSection: platformAttribute version: #notDefined;
		commitMethod;
		symbolicVersionMethod: #stable;
		addSymbolicSection: platformAttribute version: developmentVersionString;
		commitMethod.
	^ toolbox
]

{ #category : #scripts }
MetacelloToolBox class >> saveConfigurationPackageFor: configurationBasename description: commitMessage [
	"Save mcz file that contains the configuration to it's repository."
	
	<apiDocumentation>
	| toolbox criticalIssues |
	(criticalIssues := self checkForCriticalValidationIssues: configurationBasename) isEmpty
		ifTrue: [ 
			toolbox := self configurationNamed: configurationBasename.
			toolbox project goferCommitProject: commitMessage.
			^toolbox ].
	 self error: 'There are critical issues in configuration ' , configurationBasename printString, '. Configuration not saved'.
	^nil
]

{ #category : #scripts }
MetacelloToolBox class >> saveModifiedDevelopmentPackages: developmentVersionString for: configurationBasename description: commitMessage [
  "Save modified mcz files associated with the project. Then update given version to reflect the new mcz file versions."

  <apiDocumentation>
  | toolbox modifiedPackages cacheRepository |
  modifiedPackages := Dictionary new.
  (toolbox := self configurationNamed: configurationBasename)
    modifiedPackageSpecs: developmentVersionString
    packageSpecsDo: [ :versionSpec :packageSpec :monticelloWorkingCopy | 
      | repositories repositoryGroup wcRepositoryGroup |
      wcRepositoryGroup := monticelloWorkingCopy repositoryGroup.
      repositoryGroup := MCRepositoryGroup new.
      repositories := packageSpec repositories
        collect: [ :each | each createRepository ].
      repositories isEmpty
        ifTrue: [ 
          repositories := versionSpec repositories
            collect: [ :repoSpec | repoSpec createRepository ].
          wcRepositoryGroup repositories notEmpty
            ifTrue: [ 
              repositories := repositories
                select: [ :repo | wcRepositoryGroup includes: repo ] ].
          repositories isEmpty
            ifTrue: [ 
              repositories := monticelloWorkingCopy repositoryGroup repositories
                reject: [ :repo | repo = MetacelloPlatform current defaultPackageCache ] ] ].
      repositories
        do: [ :repo | 
          (monticelloWorkingCopy possiblyNewerVersionsIn: repo) notEmpty
            ifTrue: [ 
              self
                notify:
                  'There are possibly newer versions of the package '
                    , packageSpec name printString , ' in the repository '
                    , repo description printString
                    ,
                      '. Cancel and manually merge if you want to pick up the changes from the later version.' ].
          repositoryGroup addRepository: repo ].
      modifiedPackages at: packageSpec name put: repositoryGroup ].
  cacheRepository := MetacelloPlatform current defaultPackageCache.
  modifiedPackages isEmpty
    ifTrue: [ self notify: 'Empty modified packages list'	"actually here for debugging purposes" ].
  modifiedPackages
    keysAndValuesDo: [ :packageName :repositoryGroup | 
      | gofer repositoryList packageSpec |
      gofer := Gofer new.
      gofer disablePackageCache.
      (repositoryGroup repositories reject: [ :repo | repo = cacheRepository ])
        do: [ :repository | gofer repository: repository ].
      gofer package: packageName.
      gofer commit: commitMessage ].
  ^ modifiedPackages
]

{ #category : #'development support' }
MetacelloToolBox class >> saveModifiedPackagesAndConfigurationIn: aConfigurationClass description: commitMessage [
	"Save modified mcz files, update and then save the configuration."

	<apiDocumentation>
	| version |
	version := aConfigurationClass project version: #development.
	self
		saveModifiedDevelopmentPackages: version versionString
		for: aConfigurationClass name asString
		description: commitMessage.
	(self
		updateDevelopment: version versionString
		for: aConfigurationClass name asString
		updateProjects: false
		description: commitMessage) isEmpty
		ifTrue: [ self updateDescription: version versionString for: aConfigurationClass name asString description: commitMessage ].
	self saveConfigurationPackageFor: aConfigurationClass name asString description: commitMessage
]

{ #category : #'development support' }
MetacelloToolBox class >> saveModifiedPackagesIn: aConfigurationClass description: commitMessage [
	"Save modified mcz files and update the configuration."

	| version |
	version := aConfigurationClass project version: #development.
	self
		saveModifiedDevelopmentPackages: version versionString for: aConfigurationClass name asString description: commitMessage;
		updateDevelopment: version versionString
			for: aConfigurationClass name asString
			updateProjects: false
			description: commitMessage
]

{ #category : #scripts }
MetacelloToolBox class >> stableVersion: stableVersionString for: configurationBasename platformAttribute: platformAttribute [
	<apiDocumentation>
	"Set the #stable symbolic version to the given version, using the given platformAttribute."
	
	(MetacelloToolBox configurationNamed: configurationBasename)
		symbolicVersionMethod: #stable;
		addSymbolicSection: platformAttribute version: stableVersionString;
		commitMethod.

]

{ #category : #private }
MetacelloToolBox class >> stripVersionStringForSelector: versionString [

	^((versionString copyWithout: $.) copyWithout: $-) 
		copyReplaceAll: 'baseline' with: '' 

]

{ #category : #scripts }
MetacelloToolBox class >> updateDescription: developmentVersionString for: configurationBasename description: message [
	"Update description"

	<apiDocumentation>
	^ (self configurationNamed: configurationBasename)
		updateVersionMethodForVersion: developmentVersionString
			updateProjects: false
			updatePackages: false
			versionSpecsDo: [ :attribute :versionSpec | 
				attribute == #common
					ifTrue: [ 
						"update the author, description and timeStamp"
						versionSpec
							author: MetacelloPlatform current authorName;
							timestamp: MetacelloPlatform current timestamp.
						self appendDescription: message to: versionSpec ].
				true ];
		commitMethod
]

{ #category : #scripts }
MetacelloToolBox class >> updateDevelopment: developmentVersionString for: configurationBasename updatePackages: updatePackages description: message [
	"Update project specs and (conditionally) the package specs in the given version to reflect the new mcz file and project versions."

	<apiDocumentation>
	| toolbox updatedSpecs |
	toolbox := self configurationNamed: configurationBasename.
	updatedSpecs := toolbox
		updateVersionMethodForVersion: developmentVersionString
		updateProjects: true
		updatePackages: updatePackages
		versionSpecsDo: [ :attribute :versionSpec | 
			attribute == #common
				ifTrue: [ 
					"update the author, description and timeStamp"
					versionSpec
						author: MetacelloPlatform current authorName;
						timestamp: MetacelloPlatform current timestamp.
					self appendDescription: message to: versionSpec ].
			true ].
	updatedSpecs notEmpty
		ifTrue: [ toolbox commitMethod ].
	^ updatedSpecs
]

{ #category : #scripts }
MetacelloToolBox class >> updateDevelopment: developmentVersionString for: configurationBasename updateProjects: updateProjects description: message [
	"Update packageSpecs and (conditionally) the project specs in the given version to reflect the new mcz file and project versions."

	<apiDocumentation>
	| toolbox updatedSpecs |
	toolbox := self configurationNamed: configurationBasename.
	updatedSpecs := toolbox
		updateVersionMethodForVersion: developmentVersionString
		updateProjects: updateProjects
		updatePackages: true
		versionSpecsDo: [ :attribute :versionSpec | 
			attribute == #common
				ifTrue: [ 
					"update the author, description and timeStamp"
					versionSpec
						author: MetacelloPlatform current authorName;
						timestamp: MetacelloPlatform current timestamp.
					self appendDescription: message to: versionSpec ].
			true ].
	updatedSpecs notEmpty
		ifTrue: [ toolbox commitMethod ].
	^ updatedSpecs
]

{ #category : #'development support' }
MetacelloToolBox class >> updateToLatestPackageVersionsIn: aConfigurationClass description: descriptionString [
	"Update the #development version to match currently loaded mcz files."

	"self updateToLatestPackageVersions: '- fixed a bug'"

	<apiDocumentation>
	^self
		updateDevelopment: #development
		for: aConfigurationClass name asString
		updateProjects: false 
		description: descriptionString
]

{ #category : #validation }
MetacelloToolBox class >> validateBaseline: baselineClass [
  "Check the baseline for Errors, Critical Warnings, and Warnings (see class comment for MetacelloMCVersionValidator for more information). "

  <apiDocumentation>
  ^ self validateBaseline: baselineClass debug: #() recurse: false
]

{ #category : #validation }
MetacelloToolBox class >> validateBaseline: baselineClass debug: debugList recurse: aBool [
  "Check the baseline for Errors, Critical Warnings, and Warnings (see class comment for MetacelloMCVersionValidator for more information). "

  <apiDocumentation>
  ^ MetacelloMCVersionValidator
    validateBaseline: baselineClass
    debug: debugList
    recurse: aBool
]

{ #category : #validation }
MetacelloToolBox class >> validateConfiguration: configurationClass [
	"Check the configuration for Errors, Critical Warnings, and Warnings (see class comment for MetacelloMCVersionValidator for more information). "

	<apiDocumentation>
	^ self validateConfiguration: configurationClass debug: #() recurse: false
]

{ #category : #validation }
MetacelloToolBox class >> validateConfiguration: configurationClass debug: debugList recurse: aBool [
	"Check the configuration for Errors, Critical Warnings, and Warnings (see class comment for MetacelloMCVersionValidator for more information). "

	<apiDocumentation>
	^ MetacelloMCVersionValidator validateConfiguration: configurationClass debug: debugList recurse: aBool
]

{ #category : #'load validation' }
MetacelloToolBox class >> validatePlatformLoad: platformAttribute for: configurationBasename [
	"Validate, Load, and run tests for all versions of the given configuration. Return list of issues."

	<apiDocumentation>
	| issues configurationClass toolbox cleanDevelopmentLoads cleanLoadAndTests stableVersion |
	configurationClass := self environment at: (self configurationNameFrom: configurationBasename) asSymbol.
	cleanDevelopmentLoads := OrderedCollection new.
	cleanLoadAndTests := OrderedCollection new.
	issues := [ MetacelloMCVersionValidator validateConfigurationLoad: configurationClass ]
		on: MetacelloCleanNotification
		do: [ :ex | 
			(ex isKindOf: MetacelloCleanLoadAndTestsNotification)
				ifTrue: [ cleanLoadAndTests add: ex version ].
			((ex isKindOf: MetacelloCleanLoadNotification) and: [ ex version blessing == #development ])
				ifTrue: [ cleanDevelopmentLoads add: ex version ].
			ex resume ].
	(cleanDevelopmentLoads isEmpty and: [ cleanLoadAndTests isEmpty ])
		ifTrue: [ 
			self inform: 'Failed validation with no versions load clean'.
			^ issues ].
	toolbox := self configurationNamed: configurationBasename.
	cleanLoadAndTests notEmpty
		ifTrue: [ 
			toolbox
				symbolicVersionMethod: #stable;
				addSymbolicSection: platformAttribute version: (stableVersion := cleanLoadAndTests last) versionString;
				commitMethod ].
	stableVersion notNil
		ifTrue: [ 
			stableVersion blessing == #development
				ifTrue: [ 
					issues
						add:
							(MetacelloValidationWarning
								configurationClass: configurationClass
								reasonCode: #stableDevelopmentVersion
								callSite: #validatePlatformLoad:for:
								explanation:
									'Development version ' , stableVersion versionString printString , ' is marked as #stableVersion for '
										, configurationClass name asString) ].
			cleanDevelopmentLoads := cleanDevelopmentLoads select: [ :version | version > stableVersion ] ].
	cleanDevelopmentLoads notEmpty
		ifTrue: [ 
			toolbox
				symbolicVersionMethod: #development;
				addSymbolicSection: platformAttribute version: cleanDevelopmentLoads last versionString;
				commitMethod ]
		ifFalse: [ 
			toolbox
				symbolicVersionMethod: #development;
				addSymbolicSection: platformAttribute version: #notDefined;
				commitMethod ].
	^ issues
]

{ #category : #'load validation' }
MetacelloToolBox class >> validatePlatformLoad: platformAttribute for: configurationBasename version: versionString [
	"Load, and run tests for the given version of the configuration. Mark as #stable, if load and tests are clean. Return list of issues."

	<apiDocumentation>
	| issues configurationClass toolbox cleanLoadAndTests stableVersion |
	configurationClass := self environment at: (self configurationNameFrom: configurationBasename) asSymbol.
	cleanLoadAndTests := OrderedCollection new.
	issues := [ MetacelloMCVersionValidator validateConfigurationLoad: configurationClass version: versionString ]
		on: MetacelloCleanNotification
		do: [ :ex | 
			(ex isKindOf: MetacelloCleanLoadAndTestsNotification)
				ifTrue: [ cleanLoadAndTests add: ex version ].
			ex resume ].
	toolbox := self configurationNamed: configurationBasename.
	cleanLoadAndTests isEmpty
		ifTrue: [ ^issues ].
	stableVersion := cleanLoadAndTests last.
	toolbox
		symbolicVersionMethod: #stable;
		addSymbolicSection: platformAttribute version: stableVersion versionString;
		commitMethod.
	stableVersion blessing == #development
		ifTrue: [ 
			issues
				add:
					(MetacelloValidationWarning
						configurationClass: configurationClass
						reasonCode: #stableDevelopmentVersion
						callSite: #validatePlatformLoad:for:version:
						explanation:
							'Development version ' , stableVersion versionString printString , ' is marked as #stableVersion for '
								, configurationClass name asString) ].
	^ issues
]

{ #category : #validation }
MetacelloToolBox class >> validateProject: aMetacelloProject version: versionString [
	"Check a specific version in the configuration for Errors, Critical Warnings, and Warnings (see class comment for MetacelloMCVersionValidator for more information). "

	<apiDocumentation>
	^ self validateProject: aMetacelloProject version: versionString debug: #() recurse: false

]

{ #category : #validation }
MetacelloToolBox class >> validateProject: aMetacelloProject version: versionString debug: debugList recurse: aBool [
	"Check a specific version in the configuration for Errors, Critical Warnings, and Warnings (see class comment for MetacelloMCVersionValidator for more information). "

	<apiDocumentation>
	^ MetacelloMCVersionValidator
		validateProject: aMetacelloProject
		version: versionString
		debug: debugList
		recurse: aBool
]

{ #category : #'api-configuration' }
MetacelloToolBox >> abortMethod [

	project := project configuration class project.	"recalculate project"
	methodSpec := nil
]

{ #category : #'api-configuration' }
MetacelloToolBox >> addBaselineOfSection: sectionAttributeOrPath requiredProjects: projectList packages: packageList dependencies: dependecies groups: groups versionSpecsDo: aBlock [
  "
		repositoryDescription - metacello repository string
		projectList - collection of project base names (without ConfigurationOf prefix)
		packageList - collection of package base names
		dependencies - collection of associations where key is name of package and value is collection of required packages and projects
		groups - collection of associations where key is the name of the group and value is the collection of group members
	"

  self
    addBaselineOfSection: sectionAttributeOrPath
    requiredProjects: projectList
    packages: packageList
    dependencies: dependecies
    includes: #()
    files: #()
    repositories: #()
    preLoadDoIts: #()
    postLoadDoIts: #()
    supplyingAnswers: #()
    groups: groups
    versionSpecsDo: aBlock
]

{ #category : #'api-configuration' }
MetacelloToolBox >> addBaselineOfSection: sectionAttributeOrPath requiredProjects: projectList packages: packageList dependencies: dependecies includes: includes files: files repositories: repositories preLoadDoIts: preLoadDoIts postLoadDoIts: postLoadDoIts supplyingAnswers: supplyingAnswers groups: groups versionSpecsDo: aBlock [
  "
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

  | versionSpec dependencyMap includesMap filesMap repositoriesMap preLoadDoItsMap postLoadDoItsMap supplyingAnswersMap |
  versionSpec := self createVersionSpec: self methodSpec versionString.
  dependencyMap := self buildMapFrom: dependecies for: packageList.
  includesMap := self buildMapFrom: includes for: packageList.
  filesMap := self buildMapFrom: files for: packageList.
  repositoriesMap := self
    buildMapFrom: repositories
    for: packageList , projectList.
  preLoadDoItsMap := self buildMapFrom: preLoadDoIts for: packageList.
  postLoadDoItsMap := self buildMapFrom: postLoadDoIts for: packageList.
  supplyingAnswersMap := self buildMapFrom: supplyingAnswers for: packageList.
  projectList
    do: [ :projectName | 
      | spec |
      spec := self createProjectReferenceSpec: projectName.
      repositoriesMap
        at: projectName
        ifPresent: [ :repositoriesList | repositoriesList do: [ :repoString | spec repository: repoString ] ].
      versionSpec packages merge: spec ].
  packageList
    do: [ :packageName | 
      | spec |
      spec := self createPackageSpec: packageName.
      dependencyMap
        at: packageName
        ifPresent: [ :dependencyList | spec requires: dependencyList ].
      includesMap
        at: packageName
        ifPresent: [ :includesList | spec includes: includesList ].
      filesMap at: packageName ifPresent: [ :file | spec file: file ].
      repositoriesMap
        at: packageName
        ifPresent: [ :repositoriesList | repositoriesList do: [ :repoString | spec repository: repoString ] ].
      preLoadDoItsMap
        at: packageName
        ifPresent: [ :preLoad | spec preLoadDoIt: preLoad ].
      postLoadDoItsMap
        at: packageName
        ifPresent: [ :postLoad | spec postLoadDoIt: postLoad ].
      supplyingAnswersMap
        at: packageName
        ifPresent: [ :answer | spec answers: answer ].
      versionSpec packages merge: spec ].
  groups
    do: [ :assoc | 
      | spec |
      spec := self createGroupSpec: assoc key.
      spec includes: assoc value.
      versionSpec packages merge: spec ].
  aBlock value: versionSpec.
  self methodSpec
    addMethodSection: sectionAttributeOrPath asMetacelloAttributePath
    versionSpec: versionSpec
]

{ #category : #'api-configuration' }
MetacelloToolBox >> addBaselineOfSection: sectionAttributeOrPath requiredProjects: projectList packages: packageList repositories: repositories dependencies: dependecies groups: groups versionSpecsDo: aBlock [
  "
		repositoryDescription - metacello repository string
		projectList - collection of project base names (without ConfigurationOf prefix)
		packageList - collection of package base names
		repositories - collection of associations where key is name of package and value is collection of repository descriptions
		dependencies - collection of associations where key is name of package and value is collection of required packages and projects
		groups - collection of associations where key is the name of the group and value is the collection of group members
	"

  self
    addBaselineOfSection: sectionAttributeOrPath
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
]

{ #category : #'api-configuration' }
MetacelloToolBox >> addSection: sectionAttributeOrPath repository: repositoryDescription requiredProjects: projectList packages: packageList dependencies: dependecies groups: groups versionSpecsDo: aBlock [
	"
		repositoryDescription - metacello repository string
		projectList - collection of project base names (without ConfigurationOf prefix)
		packageList - collection of package base names
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
		repositories: #()
		preLoadDoIts: #()
		postLoadDoIts: #()
		supplyingAnswers: #()
		groups: groups
		versionSpecsDo: aBlock
]

{ #category : #'api-configuration' }
MetacelloToolBox >> addSection: sectionAttributeOrPath repository: repositoryDescription requiredProjects: projectList packages: packageList dependencies: dependecies includes: includes files: files repositories: repositories preLoadDoIts: preLoadDoIts postLoadDoIts: postLoadDoIts supplyingAnswers: supplyingAnswers groups: groups versionSpecsDo: aBlock [
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
    projectList
        do: [ :projectName | 
            | spec |
            spec := self createProjectReferenceSpec: projectName.
            repositoriesMap
                at: projectName
                ifPresent: [ :repositoriesList | repositoriesList do: [ :repoString | spec repository: repoString ] ].
            versionSpec packages merge: spec ].
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
]

{ #category : #'api-configuration' }
MetacelloToolBox >> addSection: sectionAttributeOrPath repository: repositoryDescription requiredProjects: projectList packages: packageList repositories: repositories dependencies: dependecies groups: groups versionSpecsDo: aBlock [
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
]

{ #category : #'api-configuration' }
MetacelloToolBox >> addSectionsFrom: inputVersionStringOrSymbol forBaseline: forBaseline updateProjects: updateProjects updatePackages: updatePackages versionSpecsDo: aBlock [
    | constructor updatedSpecs projectAttributes fullVersionSpec version coll pragma imports visitedSpecs fromVersionString |
    constructor := self constructor.
    updatedSpecs := Set new.
    visitedSpecs := Set new.
    projectAttributes := project attributes.
    version := project version: inputVersionStringOrSymbol.
    fromVersionString := version versionString.
    fullVersionSpec := version spec.
    fullVersionSpec := version spec.
    coll := constructor extractAllVersionPragmas at: fromVersionString ifAbsent: [ ^ nil ].
    coll size > 1
        ifTrue: [ self error: 'More than one pragma defining ' , fromVersionString printString ].
    pragma := coll at: 1.
    imports := pragma numArgs = 2
        ifTrue: [ pragma argumentAt: 2 ]
        ifFalse: [ #() ].
    methodSpec imports: imports.
    constructor extractMethodSectionsFor: fromVersionString.
    constructor methodSections
        do: [ :methodSection | 
            | versionSpec attributeList |
            versionSpec := methodSection versionSpec.
            attributeList := methodSection attributes.
            forBaseline
                ifFalse: [ 
                    (projectAttributes includes: attributeList)
                        ifTrue: [ 
                            self
                                updateVersionSpec: versionSpec
                                fullVersionSpec: fullVersionSpec
                                updateProjects: updateProjects
                                updatePackages: updatePackages
                                visited: visitedSpecs
                                updated: updatedSpecs ] ].
            (aBlock value: methodSection attributeOrPath value: versionSpec)
                ifTrue: [ self methodSpec addMethodSection: methodSection attributePath versionSpec: versionSpec ] ]
]

{ #category : #'api-configuration' }
MetacelloToolBox >> addSymbolicSection: attribute version: versionString [
    self methodSpec methodSections
        do: [ :methodSection | 
            methodSection attributes = {attribute}
                ifTrue: [ 
                    methodSection versionString: versionString.
                    ^ self ] ].
    self methodSpec addMethodSection: attribute versionString: versionString
]

{ #category : #private }
MetacelloToolBox >> baselineNameFrom: baseName [
  ^ self class baselineNameFrom: baseName
]

{ #category : #'api-configuration' }
MetacelloToolBox >> baselineNamed: baseName [
  | baselineName |
  "Check if the class does not exist"
  baselineName := self baselineNameFrom: baseName.
  (Smalltalk includesKey: baselineName asSymbol)
    ifFalse: [ 
      "Create the configuration class"
      self ensureMetacello.
      BaselineOf
        subclass: baselineName asSymbol
        instanceVariableNames: ''
        classVariableNames: ''
        poolDictionaries: ''
        category: baselineName.
      MetacelloPlatform current registerPackageNamed: baselineName.
      (MetacelloPlatform current globalNamed: baselineName asSymbol)
        compile: 'baseline: spec <baseline>'
        classified: 'baseline'	"a bit of a hack here to create an initial null basline method" ].
  project := (MetacelloPlatform current globalNamed: baselineName asSymbol)
    project
]

{ #category : #private }
MetacelloToolBox >> buildMapFrom: mapList for: packageList [
	| map |
	map := Dictionary new.
	mapList
		do: [ :assoc | 
			| pkgName pkgSpec |
			pkgName := assoc key.
			(packageList includes: pkgName)
				ifFalse: [ self error: 'package ' , pkgName printString , ' not found in packages.' ].
			map at: pkgName put: assoc value ].
	^ map
]

{ #category : #'spec creation' }
MetacelloToolBox >> classicCreateProjectReferenceSpec: projectBaseName [
  | configurationName projectReferenceProject versionString |
  configurationName := self configurationNameFrom: projectBaseName.
  projectReferenceProject := (MetacelloPlatform current
    globalNamed: configurationName asSymbol
    ifAbsent: [ 
      self
        error:
          'The project with configuration ' , configurationName printString
            , ' for the project ' , projectBaseName printString
            , ' is not currently loaded into the image.' ])
    project.
  versionString := (projectReferenceProject hasVersion: #'stable')
    ifTrue: [ #'stable' ]
    ifFalse: [ #'bleedingEdge' ].
  ^ project projectReferenceSpec
    name: projectBaseName;
    projectReference:
        (project projectSpec
            name: projectBaseName;
            className: configurationName;
            versionString: versionString;
            yourself);
    yourself
]

{ #category : #private }
MetacelloToolBox >> clearVersionSpec: versionSpec fullVersionSpec: fullVersionSpec updateProjects: updateProjects updatePackages: updatePackages [
	versionSpec
		specListProjectDo: [ :spec | 
			updateProjects
				ifTrue: [ 
					| fullProjectReferenceSpec fullProjectSpec |
					(fullProjectReferenceSpec := fullVersionSpec packageNamed: spec name) ~~ nil
						ifTrue: [ 
							fullProjectSpec := fullProjectReferenceSpec projectReference.
							fullProjectSpec versionString == nil
								ifTrue: [ spec projectReference versionString: '' ] ] ] ]
		packageDo: [ :spec | 
			updatePackages
				ifTrue: [ 
					spec getFile ~~ nil
						ifTrue: [ spec file: '' ] ] ]
		groupDo: [ :ignored |  ]
]

{ #category : #'api-configuration' }
MetacelloToolBox >> commitBaselineOfMethod [
  | skipValidation |
  skipValidation := false.
  self methodSpec ifNil: [ ^ self ].
  self methodSpec compileMethod.
  project := project configuration class project.	"recalculate project"
  [ project version: self methodSpec versionString ]
    on: MetacelloSymbolicVersionDoesNotExistError
    do: [ :ex | 
      "symbolic versions can be removed"
      skipValidation := true ].
  skipValidation
    ifFalse: [ self validateVersion: self methodSpec versionString ].
  methodSpec := nil

]

{ #category : #'api-configuration' }
MetacelloToolBox >> commitConfiguration: commitComment [
]

{ #category : #'api-configuration' }
MetacelloToolBox >> commitMethod [
	| skipValidation |
	skipValidation := false.
	self methodSpec ifNil: [ ^self ].
	self methodSpec compileMethod.
	project := project configuration class project.	"recalculate project"
	[ project version: self methodSpec versionString ]
		on: MetacelloSymbolicVersionDoesNotExistError
		do: [ :ex | 
			"symbolic versions can be removed"
			skipValidation := true ].
	skipValidation
		ifFalse: [ self validateVersion: self methodSpec versionString ].
	methodSpec := nil

]

{ #category : #private }
MetacelloToolBox >> configurationNameFrom: baseName [	
	^self class configurationNameFrom: baseName
]

{ #category : #'api-configuration' }
MetacelloToolBox >> configurationNamed: baseName [
  | configurationName |
  "Check if the class does not exist"
  configurationName := self configurationNameFrom: baseName.
  (Smalltalk includesKey: configurationName asSymbol)
    ifFalse: [ 
      self ensureMetacello.	"Create the configuration class"
      MetacelloPlatform current
        copyClass:
          (MetacelloPlatform current globalNamed: #'MetacelloConfigTemplate')
        as: configurationName asSymbol
        inCategory: configurationName asString.	"Create the package that has the same name"
      MetacelloPlatform current registerPackageNamed: configurationName ].
  project := (MetacelloPlatform current globalNamed: configurationName asSymbol)
    project
]

{ #category : #accessing }
MetacelloToolBox >> constructor [

	| constructor |
	constructor := MetacelloToolBoxConstructor new.
	constructor configuration: project configuration class new.
	^constructor
]

{ #category : #'api-configuration' }
MetacelloToolBox >> createBaselineOfMethod: selector inCategory: category [
  methodSpec := MetacelloBaselineOfMethodSpec new
    project: project;
    selector: selector;
    category: category;
    yourself
]

{ #category : #'spec creation' }
MetacelloToolBox >> createConfiguration: baseName [
  | configurationName |
  "Check if the class does not exist"
  configurationName := (baseName beginsWith: 'ConfigurationOf')
    ifTrue: [ baseName ]
    ifFalse: [ 'ConfigurationOf' , baseName ].
  (Smalltalk includesKey: configurationName asSymbol)
    ifFalse: [ 
      self ensureMetacello.	"Create the configuration class"
      MetacelloPlatform current
        copyClass:
          (MetacelloPlatform current globalNamed: #'MetacelloConfigTemplate')
        as: configurationName asSymbol
        inCategory: configurationName asString.	"Create the package that has the same name"
      MetacelloPlatform current registerPackageNamed: configurationName ].
  ^ (MetacelloPlatform current globalNamed: configurationName asSymbol) project
]

{ #category : #'spec creation' }
MetacelloToolBox >> createGroupSpec: baseName [
	^ (self project groupSpec)
		name: baseName;
		yourself
]

{ #category : #'spec creation' }
MetacelloToolBox >> createPackageSpec: baseName [
	^ (project packageSpec)
		name: baseName;
		yourself
]

{ #category : #'spec creation' }
MetacelloToolBox >> createProjectReferenceSpec: projectBaseName [
  | projectSpec |
  projectSpec := (Metacello image
    project: projectBaseName;
    list)
    detect: [ :each | true ]
    ifNone: [ ^ self classicCreateProjectReferenceSpec: projectBaseName ].
  ^ project projectReferenceSpec
    name: projectBaseName;
    projectReference: projectSpec;
    yourself
]

{ #category : #'api-configuration' }
MetacelloToolBox >> createSymbolicVersionMethod: selector inCategory: category forVersion: versionSymbol [
	| constructor |
	methodSpec := (MetacelloSymbolicVersionMethodSpec new)
		project: project;
		selector: selector;
		category: category;
		versionString: versionSymbol;
		yourself.
	constructor := self constructor.
	self methodSpec methodSections: (constructor extractSymbolicVersionSpecsFor: versionSymbol) asOrderedCollection.

]

{ #category : #'spec creation' }
MetacelloToolBox >> createVersion: versionString [
	| version  |
	[ version := project version: versionString ]
		on: MetacelloVersionDoesNotExistError
		do: [ :ex | 
			| versionSpec |
			versionSpec := self createVersionSpec: versionString.
			version := versionSpec createVersion.
			project map == nil
				ifTrue: [ project map: Dictionary new ].
			project map at: versionString put: version ].
	^version
]

{ #category : #'api-configuration' }
MetacelloToolBox >> createVersionMethod: selector inCategory: category forVersion: versionString [
	
	methodSpec := (MetacelloVersionMethodSpec new)
		project: project;
		selector: selector;
		category: category;
		versionString: versionString;
		yourself.
]

{ #category : #'spec creation' }
MetacelloToolBox >> createVersionSpec: versionString [
	| versionSpec |
	versionSpec := project versionSpec.
	versionSpec versionString: versionString.
	^ versionSpec
]

{ #category : #private }
MetacelloToolBox >> editVersionSpecsForImport: constructor projectAttributes: projectAttributes active: activeSection fullVersionSpec: fullVersionSpec updateProjects: updateProjects updatePackages: updatePackages visited: visitedSpecs updated: updatedSpecs [
    | fileSpecified encounteredPackageSpecs |
    fileSpecified := Bag new.
    encounteredPackageSpecs := Bag new.
    constructor
        methodSectionsInEvaluationOrder: projectAttributes reverse
        do: [ :methodSection | 
            "collect packageSpec names ... interested in singletons"
            methodSection versionSpec
                specListProjectDo: [ :spec |  ]
                packageDo: [ :spec | 
                    encounteredPackageSpecs add: spec name.
                    (spec copy updateForSpawnMethod: spec)
                        ifFalse: [ fileSpecified add: spec name ] ]
                groupDo: [ :spec |  ] ].
    constructor
        methodSectionsInEvaluationOrder: projectAttributes reverse
        do: [ :methodSection | 
            | versionSpec attributePath packageSpecsToDelete |
            versionSpec := methodSection versionSpec.
            attributePath := methodSection attributePath.
            versionSpec updateForSpawnMethod: versionSpec.
            packageSpecsToDelete := OrderedCollection new.
            versionSpec
                specListProjectDo: [ :spec | spec updateForSpawnMethod: spec ]
                packageDo: [ :spec | 
                    encounteredPackageSpecs remove: spec name.
                    (spec updateForSpawnMethod: spec)
                        ifTrue: [ 
                            "no file speced"
                            ((fileSpecified includes: spec name) or: [ encounteredPackageSpecs includes: spec name ])
                                ifTrue: [ 
                                    "already specified somewhere else, need to delete this spec from version spec OR we're deleting all but first occurence"
                                    packageSpecsToDelete add: spec ]
                                ifFalse: [ 
                                    "file hasn't been specified, so specify now"
                                    spec file: spec name.
                                    fileSpecified add: spec name ] ]
                        ifFalse: [ 
                            "file specified"
                            (fileSpecified occurrencesOf: spec name) > 1
                                ifTrue: [ 
                                    "multiple specifications for file ... delete all but first occurence"
                                    packageSpecsToDelete add: spec.
                                    fileSpecified remove: spec name ] ] ]
                groupDo: [ :spec | spec updateForSpawnMethod: spec ].
            packageSpecsToDelete do: [ :spec | versionSpec deleteSpec: spec ].
            activeSection
                ifTrue: [ 
                    self
                        updateVersionSpec: versionSpec
                        fullVersionSpec: fullVersionSpec
                        updateProjects: updateProjects
                        updatePackages: updatePackages
                        visited: visitedSpecs
                        updated: updatedSpecs ]
                ifFalse: [ 
                    self
                        clearVersionSpec: versionSpec
                        fullVersionSpec: fullVersionSpec
                        updateProjects: updateProjects
                        updatePackages: updatePackages ] ]
]

{ #category : #private }
MetacelloToolBox >> ensureMetacello [
	"noop for now"
]

{ #category : #testing }
MetacelloToolBox >> hasMethodForSymbolicVersion: versionSymbol [

	^(self constructor extractSymbolicVersionPragmas at: versionSymbol ifAbsent: [ ^ false ]) size > 0

]

{ #category : #'api-configuration' }
MetacelloToolBox >> importFrom: inputVersionStringOrSymbol projectAttributes: projectAttributes inactiveAttributes: inactiveAttributesList updateProjects: updateProjects updatePackages: updatePackages versionSpecsDo: aBlock [
    | constructor updatedSpecs fullVersionSpec version visitedSpecs fromVersionString attributeSet |
    constructor := self constructor.
    updatedSpecs := Set new.
    visitedSpecs := Set new.
    version := project version: inputVersionStringOrSymbol.
    fromVersionString := version versionString.	"resolve symbolic versions"
    fullVersionSpec := version spec.
    self methodSpec imports: {fromVersionString}.
    constructor extractMethodSectionsFor: fromVersionString.
    self
        editVersionSpecsForImport: constructor
        projectAttributes: projectAttributes
        active: true
        fullVersionSpec: fullVersionSpec
        updateProjects: updateProjects
        updatePackages: updatePackages
        visited: visitedSpecs
        updated: updatedSpecs.
    attributeSet := projectAttributes.
    inactiveAttributesList
        do: [ :inactiveAttributes | 
            self
                editVersionSpecsForImport: constructor
                projectAttributes: inactiveAttributes
                active: false
                fullVersionSpec: fullVersionSpec
                updateProjects: updateProjects
                updatePackages: updatePackages
                visited: visitedSpecs
                updated: updatedSpecs.
            attributeSet := attributeSet , projectAttributes ].
    attributeSet := attributeSet asSet.
    constructor
        methodSectionsDo: [ :methodSection | 
            | versionSpec attributePath |
            versionSpec := methodSection versionSpec.
            attributePath := methodSection attributePath.
            (methodSection includesAttributeFrom: attributeSet)
                ifFalse: [ 
                    versionSpec updateForSpawnMethod: versionSpec.
                    versionSpec
                        specListProjectDo: [ :spec | spec updateForSpawnMethod: spec ]
                        packageDo: [ :spec | spec updateForSpawnMethod: spec ]
                        groupDo: [ :spec | spec updateForSpawnMethod: spec ].
                    self
                        clearVersionSpec: versionSpec
                        fullVersionSpec: fullVersionSpec
                        updateProjects: updateProjects
                        updatePackages: updatePackages ].
            (aBlock value: attributePath asAttributeOrPath value: versionSpec)
                ifTrue: [ self methodSpec addMethodSection: attributePath versionSpec: versionSpec ] ]
]

{ #category : #'api-configuration' }
MetacelloToolBox >> importFrom: inputVersionStringOrSymbol updateProjects: updateProjects updatePackages: updatePackages versionSpecsDo: aBlock [
    ^ self
        importFrom: inputVersionStringOrSymbol
        projectAttributes: project attributes
        inactiveAttributes: #()
        updateProjects: updateProjects
        updatePackages: updatePackages
        versionSpecsDo: aBlock
]

{ #category : #'api-configuration' }
MetacelloToolBox >> imports: importList [

	self methodSpec imports: importList
]

{ #category : #'api-configuration' }
MetacelloToolBox >> methodSectionsFrom: inputVersionStringOrSymbol do: methodSectionBlock [
  | constructor version fromVersionString |
  constructor := self constructor.
  version := project version: inputVersionStringOrSymbol.
  fromVersionString := version versionString.	"resolve symbolic versions"
  constructor extractMethodSectionsFor: fromVersionString.
  constructor methodSectionsDo: methodSectionBlock
]

{ #category : #accessing }
MetacelloToolBox >> methodSpec [

	^methodSpec
]

{ #category : #'api-configuration' }
MetacelloToolBox >> modifiedPackageSpecs: versionStringOrSymbol packageSpecsDo: aBlock [
	| versionSpec |
	versionSpec := (self project version: versionStringOrSymbol) spec.
	versionSpec
		projectDo: [ :ignored |  ]
		packageDo: [ :packageSpec | 
			| wc |
			wc := packageSpec workingCopy.
			wc ~~ nil
				ifTrue: [ 
					wc modified
						ifTrue: [ aBlock value: versionSpec value: packageSpec value: wc ] ] ]
		groupDo: [ :ignored |  ]
]

{ #category : #'api-configuration' }
MetacelloToolBox >> modifyBaselineOf [
  | baselineClass constructor pragma baseline |
  baselineClass := project configuration class.
  baseline := baselineClass new.
  constructor := MetacelloToolBoxBaselineConstructor new
    configuration: project configuration class new;
    yourself.
  pragma := constructor extractBaselinePragmaFor: baselineClass.
  constructor extractMethodSectionsForClass: baselineClass.
  methodSpec := MetacelloBaselineOfMethodSpec new
    project: project;
    selector: (MetacelloPlatform current selectorForPragma: pragma);
    category: (baselineClass whichCategoryIncludesSelector: (MetacelloPlatform current selectorForPragma: pragma));
    yourself.
  constructor methodSections
    do: [ :methodSection | self methodSpec methodSections add: methodSection ]
]

{ #category : #'api-configuration' }
MetacelloToolBox >> modifySection: sectionAttributeOrPath repository: repositoryDescription requiredProjects: newProjectList packages: newPackageList dependencies: dependecies includes: includes files: files repositories: repositories preLoadDoIts: preLoadDoIts postLoadDoIts: postLoadDoIts supplyingAnswers: supplyingAnswers groups: newGroups versionSpecsDo: aBlock [
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
]

{ #category : #'api-configuration' }
MetacelloToolBox >> modifySection: sectionAttributeOrPath sectionIndex: sectionIndex repository: repositoryDescription requiredProjects: newProjectList packages: newPackageList dependencies: dependecies includes: includes files: files repositories: repositories preLoadDoIts: preLoadDoIts postLoadDoIts: postLoadDoIts supplyingAnswers: supplyingAnswers groups: newGroups versionSpecsDo: aBlock [
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

    | versionSpec dependencyMap includesMap filesMap repositoriesMap preLoadDoItsMap postLoadDoItsMap supplyingAnswersMap groups packageList newGroupNames |
    versionSpec := (self methodSpec
        findMethodSection: sectionAttributeOrPath asMetacelloAttributePath
        sectionIndex: sectionIndex) versionSpec.
    repositoryDescription ~~ nil
        ifTrue: [ versionSpec repository: repositoryDescription ].
    newProjectList
        do: [ :projectName | 
            (versionSpec packages specListDetect: [ :spec | spec name = projectName ] ifNone: [  ]) == nil
                ifTrue: [ versionSpec packages merge: (self createProjectReferenceSpec: projectName) ]
                ifFalse: [ self error: 'Project named: ' , projectName printString , ' already exists.' ] ].
    packageList := OrderedCollection new.
    groups := OrderedCollection new.
    newGroupNames := OrderedCollection new.
    versionSpec
        projectDo: [ :ignored |  ]
        packageDo: [ :pkg | packageList add: pkg name ]
        groupDo: [ :group | 
            groups add: group name -> group includes.
            newGroupNames add: group name ].
    newPackageList
        do: [ :packageName | 
            (packageList includes: packageName)
                ifTrue: [ self error: 'Package named: ' , packageName printString , ' already exists.' ]
                ifFalse: [ packageList add: packageName ] ].
    newGroups
        do: [ :assoc | 
            (newGroupNames includes: assoc key)
                ifTrue: [ self error: 'Group named: ' , assoc key printString , ' already exists.' ]
                ifFalse: [ groups add: assoc ] ].
    dependencyMap := self buildMapFrom: dependecies for: packageList.
    includesMap := self buildMapFrom: includes for: packageList.
    filesMap := self buildMapFrom: files for: packageList.
    repositoriesMap := self buildMapFrom: repositories for: packageList.
    preLoadDoItsMap := self buildMapFrom: preLoadDoIts for: packageList.
    postLoadDoItsMap := self buildMapFrom: postLoadDoIts for: packageList.
    supplyingAnswersMap := self buildMapFrom: supplyingAnswers for: packageList.
    packageList
        do: [ :packageName | 
            | spec |
            (newPackageList includes: packageName)
                ifTrue: [ 
                    spec := self createPackageSpec: packageName.
                    versionSpec packages merge: spec ]
                ifFalse: [ spec := versionSpec packages specListDetect: [ :spc | spc name = packageName ] ].
            dependencyMap
                at: packageName
                ifPresent: [ :dependencyList | spec setRequires: spec requires , dependencyList ].
            includesMap at: packageName ifPresent: [ :includesList | spec setIncludes: spec includes , includesList ].
            filesMap at: packageName ifPresent: [ :file | spec file: file ].
            repositoriesMap
                at: packageName
                ifPresent: [ :repositoriesList | repositoriesList do: [ :repoString | spec repository: repoString ] ].
            preLoadDoItsMap at: packageName ifPresent: [ :preLoad | spec preLoadDoIt: preLoad ].
            postLoadDoItsMap at: packageName ifPresent: [ :postLoad | spec postLoadDoIt: postLoad ].
            supplyingAnswersMap at: packageName ifPresent: [ :answer | spec answers: answer ] ].
    groups
        do: [ :assoc | 
            | spec |
            (newGroupNames includes: assoc key)
                ifFalse: [ 
                    spec := self createGroupSpec: assoc key.
                    versionSpec packages merge: spec.
                    spec setIncludes: spec includes , assoc value ] ].
    aBlock value: versionSpec
]

{ #category : #'api-configuration' }
MetacelloToolBox >> modifySymbolicVersionMethodFor: versionSymbol [

	^self modifySymbolicVersionMethodFor: versionSymbol symbolicVersionSpecsDo: [:symbolicVersionSpec | true ].

]

{ #category : #'api-configuration' }
MetacelloToolBox >> modifySymbolicVersionMethodFor: versionSymbol symbolicVersionSpecsDo: aBlock [
    | constructor coll pragma |
    constructor := self constructor.
    coll := constructor extractSymbolicVersionPragmas at: versionSymbol ifAbsent: [ ^ nil ].
    coll size > 1
        ifTrue: [ self error: 'More than one pragma defining ' , versionSymbol printString ].
    pragma := coll at: 1.
    methodSpec := MetacelloSymbolicVersionMethodSpec new
        project: project;
        selector: (MetacelloPlatform current selectorForPragma: pragma);
        category: (project configuration class whichCategoryIncludesSelector: (MetacelloPlatform current selectorForPragma: pragma));
        versionString: versionSymbol;
        yourself.
    (constructor extractSymbolicVersionSpecsFor: versionSymbol)
        do: [ :symbolicVersionSpec | 
            (aBlock value: symbolicVersionSpec)
                ifTrue: [ self methodSpec addMethodSection: symbolicVersionSpec attributes versionString: symbolicVersionSpec versionString ] ]
]

{ #category : #'api-configuration' }
MetacelloToolBox >> modifyVersionMethodForVersion: inputVersionStringOrSymbol [
	^ self modifyVersionMethodForVersion: inputVersionStringOrSymbol versionSpecsDo: [ :attribute :versionSpec | true ]
]

{ #category : #'api-configuration' }
MetacelloToolBox >> modifyVersionMethodForVersion: inputVersionStringOrSymbol versionSpecsDo: aBlock [
	^ self
		updateVersionMethodForVersion: inputVersionStringOrSymbol
		updateProjects: false
		updatePackages: false
		versionSpecsDo: aBlock
]

{ #category : #accessing }
MetacelloToolBox >> project [

	^project
]

{ #category : #accessing }
MetacelloToolBox >> project: aMetacelloProject [

	project := aMetacelloProject
]

{ #category : #'api-configuration' }
MetacelloToolBox >> symbolicVersionMethod: versionSymbol [
	(self hasMethodForSymbolicVersion: versionSymbol)
		ifTrue: [ self modifySymbolicVersionMethodFor: versionSymbol ]
		ifFalse: [ 
			self
				createSymbolicVersionMethod: versionSymbol asString , ':'
				inCategory: 'symbolic versions'
				forVersion: versionSymbol ].

]

{ #category : #'api-configuration' }
MetacelloToolBox >> updateVersionMethodForVersion: inputVersionStringOrSymbol projectAttributes: projectAttributes updateProjects: updateProjects updatePackages: updatePackages versionSpecsDo: aBlock [
    | constructor updatedSpecs fullVersionSpec version coll pragma imports blessing addedSections visitedSpecs sourceVersionString keptSections |
    updatedSpecs := Set new.
    version := project version: inputVersionStringOrSymbol.
    sourceVersionString := version versionString.	"resolve symbolic version"
    blessing := version blessing.
    blessing == #'release'
        ifTrue: [ 
            MetacelloCannotUpdateReleasedVersionError
                signal: 'Should not update a version that has been released (resume to continue update).' ].
    fullVersionSpec := version spec.
    constructor := self constructor.
    coll := constructor extractAllVersionPragmas at: sourceVersionString ifAbsent: [ ^ updatedSpecs ].
    coll size > 1
        ifTrue: [ self error: 'More than one pragma defining ' , sourceVersionString printString ].
    pragma := coll at: 1.
    imports := pragma numArgs = 2
        ifTrue: [ pragma argumentAt: 2 ]
        ifFalse: [ #() ].
    methodSpec := MetacelloVersionMethodSpec new
        project: project;
        selector: (MetacelloPlatform current selectorForPragma: pragma);
        category: (project configuration class whichCategoryIncludesSelector: (MetacelloPlatform current selectorForPragma:  pragma));
        versionString: sourceVersionString;
        imports: imports;
        yourself.
    constructor extractMethodSectionsFor: sourceVersionString.
    addedSections := Dictionary new.
    visitedSpecs := Set new.
    keptSections := IdentitySet new.
    constructor
        methodSectionsInEvaluationOrder: projectAttributes reverse
        do: [ :methodSection | 
            | versionSpec |
            versionSpec := methodSection versionSpec.
            (blessing ~~ #'baseline' and: [ methodSection includesAttributeFrom: projectAttributes ])
                ifTrue: [ 
                    self
                        updateVersionSpec: versionSpec
                        fullVersionSpec: fullVersionSpec
                        updateProjects: updateProjects
                        updatePackages: updatePackages
                        visited: visitedSpecs
                        updated: updatedSpecs ].
            (aBlock value: methodSection attributeOrPath value: versionSpec)
                ifTrue: [ keptSections add: methodSection topParent ] ].
    constructor methodSections
        do: [ :methodSection | 
            (methodSection includesAttributeFrom: projectAttributes)
                ifTrue: [ 
                    (keptSections includes: methodSection)
                        ifTrue: [ self methodSpec methodSections add: methodSection ] ]
                ifFalse: [ self methodSpec methodSections add: methodSection ] ].
    ^ updatedSpecs

]

{ #category : #'api-configuration' }
MetacelloToolBox >> updateVersionMethodForVersion: inputVersionStringOrSymbol updateProjects: updateProjects updatePackages: updatePackages versionSpecsDo: aBlock [
    ^ self
        updateVersionMethodForVersion: inputVersionStringOrSymbol
        projectAttributes: project attributes
        updateProjects: updateProjects
        updatePackages: updatePackages
        versionSpecsDo: aBlock
]

{ #category : #'api-configuration' }
MetacelloToolBox >> updateVersionMethodForVersion: inputVersionStringOrSymbol versionSpecsDo: aBlock [
	^ self
		updateVersionMethodForVersion: inputVersionStringOrSymbol
		updateProjects: true
		updatePackages: true
		versionSpecsDo: aBlock
]

{ #category : #private }
MetacelloToolBox >> updateVersionSpec: versionSpec fullVersionSpec: fullVersionSpec updateProjects: updateProjects updatePackages: updatePackages visited: visitedSpecs updated: updatedSpecs [
    versionSpec
        specListProjectDo: [ :spec | 
            (updateProjects and: [ (visitedSpecs includes: spec name) not ])
                ifTrue: [ 
                    | fullProjectSpec |
                    fullProjectSpec := (fullVersionSpec packageNamed: spec name) projectReference.
                    (fullProjectSpec versionString ~~ nil
                        and: [ fullProjectSpec versionString ~~ #'stable' and: [ fullProjectSpec projectClass ~~ nil ] ])
                        ifTrue: [ 
                            | cv |
                            cv := fullProjectSpec projectClassProject currentVersion.
                            cv ~~ nil
                                ifTrue: [ 
                                    cv versionString ~= fullProjectSpec versionString
                                        ifTrue: [ 
                                            spec projectReference versionString: cv versionString.
                                            updatedSpecs add: spec name ] ]
                                ifFalse: [ 
                                    "set to an empty string as a place holder, since we know we should have a version, but we are unable to figure out the version"
                                    spec projectReference versionString: ''.
                                    updatedSpecs add: spec name ] ] ] ]
        packageDo: [ :spec | 
            (updatePackages and: [ (visitedSpecs includes: spec name) not ])
                ifTrue: [ 
                    spec getFile ~~ nil
                        ifTrue: [ 
                            | viName |
                            spec file = ''
                                ifTrue: [ spec file: spec name ].
                            (viName := spec workingCopyNameFor: spec loader) ~~ nil
                                ifTrue: [ 
                                    viName ~= spec file
                                        ifTrue: [ 
                                            spec file: viName.
                                            updatedSpecs add: spec name ].
                                    visitedSpecs add: spec name ] ] ] ]
        groupDo: [ :ignored |  ]
]

{ #category : #validation }
MetacelloToolBox >> validateConfiguration [
    | issues |
    issues := (MetacelloMCVersionValidator validateConfiguration: project configuration class recurse: false)
        select: [ :issue | issue isCritical ].
    issues notEmpty
        ifTrue: [ MetacelloValidationFailure issues: issues message: 'There are critical issues in the configuration' ]
]

{ #category : #validation }
MetacelloToolBox >> validateVersion: versionStringOrSymbol [
    | issues |
    issues := (MetacelloMCVersionValidator validateProject: project version: versionStringOrSymbol)
        select: [ :issue | issue isError ].
    issues notEmpty
        ifTrue: [ 
            MetacelloValidationFailure
                issues: issues
                message: 'There are error issues with version ' , versionStringOrSymbol printString ]
]

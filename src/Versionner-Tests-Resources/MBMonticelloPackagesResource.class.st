"
A test resource for test purposes for Versionner
"
Class {
	#name : #MBMonticelloPackagesResource,
	#superclass : #TestResource,
	#instVars : [
		'monticelloRepository'
	],
	#category : #'Versionner-Tests-Resources'
}

{ #category : #accessing }
MBMonticelloPackagesResource class >> isMetacelloConfig [
	"Answer true and the Metacello tools will operate on you"
	
	^true
]

{ #category : #accessing }
MBMonticelloPackagesResource class >> project [

	^self new project
]

{ #category : #accessing }
MBMonticelloPackagesResource class >> projectAttributes [

	^#()
]

{ #category : #ConfigurationOfMBFooTest }
MBMonticelloPackagesResource >> baseline10MBFooTest: spec [ 
	"self reset"
	<version: '1.0-baseline'>
	
	spec for: #common do: [
		spec repository: 'dictionary://MB_Foo_Test_Repository'.
		spec blessing: #baseline.
		spec
			package: 'MBFooTests';
			yourself ].

]

{ #category : #'command tests' }
MBMonticelloPackagesResource >> baseline20MBFooTest: spec [ 
	"self reset"
	<version: '2.0-baseline'>
	
	spec for: #common do: [
		spec repository: 'dictionary://MB_Foo_Test_Repository'.
		spec blessing: #baseline.
		spec
			package: 'MBFooTests';
			yourself ].

]

{ #category : #'command tests' }
MBMonticelloPackagesResource >> baseline30MBFooTest: spec [ 
	"self reset"
	<version: '3.0-baseline'>
	
	spec for: #common do: [
		spec repository: 'dictionary://MB_Foo_Test_Repository'.
		spec blessing: #baseline.
		spec
			package: 'MBFooTests';
			yourself ].

]

{ #category : #accessing }
MBMonticelloPackagesResource >> monticelloRepository [
	^ monticelloRepository
]

{ #category : #accessing }
MBMonticelloPackagesResource >> project [
	"self reset"

	| project |
	"Construct Metacello project"
	project := MetacelloMCProject new.
	"Allow for customization of #projectAttributes"
	project projectAttributes: (Smalltalk at: #MBMonticelloPackagesResource) projectAttributes.
	MetacelloVersionConstructor on: self project: project.
	project loader: 
		((project loaderClass new)
			shouldDisablePackageCache: true;
			yourself).
	project loadType: #linear.
	^project

]

{ #category : #running }
MBMonticelloPackagesResource >> setUp [
	"self reset"

	super setUp.
	self
		setUpMonticelloRepository;
		setUpPackagesMBFooTests;
		setUpConfigurationOfMBFooTest;
		setUpPackagesMBCommandFooTests;
		setUpConfigurationOfMBFooIssue117;
		setUpConfigurationOfMBBranchTest
]

{ #category : #ConfigurationOfMBBranchTest }
MBMonticelloPackagesResource >> setUpConfigurationOfMBBranchTest [
	"self reset"  
	| versionInfo |
	versionInfo := self setUpConfigurationOfMBBranchTest1.
	versionInfo := self setUpConfigurationOfMBBranchTest2: { versionInfo }.

]

{ #category : #ConfigurationOfMBBranchTest }
MBMonticelloPackagesResource >> setUpConfigurationOfMBBranchTest1 [
	"self reset"
	| reference className definitionArray versionInfo  |
	reference := GoferVersionReference name: 'ConfigurationOfMBBranchTests-dkh.1'.
	className := reference packageName asSymbol.
	definitionArray := {
					MCOrganizationDefinition categories: (Array with: reference packageName).
					MCClassDefinition name: className superclassName: #Object category: className instVarNames: #() comment: ''.
					MCMethodDefinition 
						className: className asString
						classIsMeta: true
						selector: 'isMetacelloConfig'
						category: 'cat'
						timeStamp: ''
						source: (self class class sourceCodeAt: #isMetacelloConfig) asString.
					MCMethodDefinition 
						className: className asString
						classIsMeta: true
						selector: 'project'
						category: 'cat'
						timeStamp: ''
						source: (self class class sourceCodeAt: #project) asString.
					MCMethodDefinition 
						className: className asString
						selector: 'project'
						category: 'cat'
						timeStamp: ''
						source: (self class sourceCodeAt: #project) asString.
					MCMethodDefinition 
						className: className asString
						selector: 'symbolicVersionDevelopmentNotDefinedMBFooTest:'
						category: 'cat'
						timeStamp: ''
						source: (self class sourceCodeAt: #symbolicVersionDevelopmentNotDefinedMBFooTest:) asString.
					MCMethodDefinition 
						className: className asString
						selector: 'baseline10MBFooTest:'
						category: 'cat'
						timeStamp: ''
						source: (self class sourceCodeAt: #baseline10MBFooTest:) asString.
					MCMethodDefinition 
						className: className asString
						selector: 'version10MBFooTest:'
						category: 'cat'
						timeStamp: ''
						source: (self class sourceCodeAt: #version10MBFooTest:) asString.
					MCMethodDefinition 
						className: className asString
						selector: 'symbolicVersionStable10MBFooTest:'
						category: 'cat'
						timeStamp: ''
						source: (self class sourceCodeAt: #symbolicVersionStable10MBFooTest:) asString.
				}.
	monticelloRepository basicStoreVersion: (MCVersion new
		setPackage:  (MCPackage new name: reference packageName)
		info: (versionInfo := MCVersionInfo
				name: reference name
				id: UUID new
				message: 'This is a mock version'
				date: Date today
				time: Time now
				author: reference author
				ancestors: #())
		snapshot: (MCSnapshot fromDefinitions: definitionArray)
		dependencies: #()).
	^versionInfo
]

{ #category : #ConfigurationOfMBBranchTest }
MBMonticelloPackagesResource >> setUpConfigurationOfMBBranchTest2: ancestors [
	"self reset"
	| reference className definitionArray versionInfo  |
	reference := GoferVersionReference name: 'ConfigurationOfMBBranchTests.branch-dkh.2'.
	className := reference packageName asSymbol.
	definitionArray := {
					MCOrganizationDefinition categories: (Array with: reference packageName).
					MCClassDefinition name: className superclassName: #Object category: className instVarNames: #() comment: ''.
					MCMethodDefinition 
						className: className asString
						classIsMeta: true
						selector: 'isMetacelloConfig'
						category: 'cat'
						timeStamp: ''
						source: (self class class sourceCodeAt: #isMetacelloConfig) asString.
					MCMethodDefinition 
						className: className asString
						classIsMeta: true
						selector: 'project'
						category: 'cat'
						timeStamp: ''
						source: (self class class sourceCodeAt: #project) asString.
					MCMethodDefinition 
						className: className asString
						selector: 'project'
						category: 'cat'
						timeStamp: ''
						source: (self class sourceCodeAt: #project) asString.
					MCMethodDefinition 
						className: className asString
						selector: 'symbolicVersionDevelopmentNotDefinedMBFooTest:'
						category: 'cat'
						timeStamp: ''
						source: (self class sourceCodeAt: #symbolicVersionDevelopmentNotDefinedMBFooTest:) asString.
					MCMethodDefinition 
						className: className asString
						selector: 'baseline10MBFooTest:'
						category: 'cat'
						timeStamp: ''
						source: (self class sourceCodeAt: #baseline10MBFooTest:) asString.
					MCMethodDefinition 
						className: className asString
						selector: 'version10MBFooTest:'
						category: 'cat'
						timeStamp: ''
						source: (self class sourceCodeAt: #version10MBFooTest:) asString.
					MCMethodDefinition 
						className: className asString
						selector: 'version11MBFooTest:'
						category: 'cat'
						timeStamp: ''
						source: (self class sourceCodeAt: #version11MBFooTest:) asString.
					MCMethodDefinition 
						className: className asString
						selector: 'symbolicVersionStable11MBFooTest:'
						category: 'cat'
						timeStamp: ''
						source: (self class sourceCodeAt: #symbolicVersionStable11MBFooTest:) asString.
				}.
	monticelloRepository basicStoreVersion: (MCVersion new
		setPackage:  (MCPackage new name: reference packageName)
		info: (versionInfo := MCVersionInfo
				name: reference name
				id: UUID new
				message: 'This is a mock version'
				date: Date today
				time: Time now
				author: reference author
				ancestors: ancestors)
		snapshot: (MCSnapshot fromDefinitions: definitionArray)
		dependencies: #()).
	^versionInfo
]

{ #category : #'command tests' }
MBMonticelloPackagesResource >> setUpConfigurationOfMBFooIssue117 [

	"self reset"  
	| versionInfo |
	versionInfo := self setUpConfigurationOfMBFooIssue117dkh1: #().
	versionInfo := self setUpConfigurationOfMBFooIssue117dkh2: #().
	versionInfo := self setUpConfigurationOfMBFooIssue117dkh3: #().

]

{ #category : #'command tests' }
MBMonticelloPackagesResource >> setUpConfigurationOfMBFooIssue117dkh1: ancestors [
	"self reset"
	| reference className definitionArray versionInfo  |
	reference := GoferVersionReference name: 'ConfigurationOfMBFooIssue117-dkh.1'.
	className := reference packageName asSymbol.
	definitionArray := {
					MCOrganizationDefinition categories: (Array with: reference packageName).
					MCClassDefinition name: className superclassName: #Object category: className instVarNames: #() comment: ''.
					MCMethodDefinition 
						className: className asString
						classIsMeta: true
						selector: 'isMetacelloConfig'
						category: 'cat'
						timeStamp: ''
						source: (self class class sourceCodeAt: #isMetacelloConfig) asString.
					MCMethodDefinition 
						className: className asString
						classIsMeta: true
						selector: 'project'
						category: 'cat'
						timeStamp: ''
						source: (self class class sourceCodeAt: #project) asString.
					MCMethodDefinition 
						className: className asString
						selector: 'project'
						category: 'cat'
						timeStamp: ''
						source: (self class sourceCodeAt: #project) asString.
					MCMethodDefinition 
						className: className asString
						selector: 'symbolicVersionDevelopment10MBFooTest:'
						category: 'cat'
						timeStamp: ''
						source: (self class sourceCodeAt: #symbolicVersionDevelopment10MBFooTest:) asString.
					MCMethodDefinition 
						className: className asString
						selector: 'baseline10MBFooTest:'
						category: 'cat'
						timeStamp: ''
						source: (self class sourceCodeAt: #baseline10MBFooTest:) asString.
					MCMethodDefinition 
						className: className asString
						selector: 'version10MBFooTest:'
						category: 'cat'
						timeStamp: ''
						source: (self class sourceCodeAt: #version10MBFooTest:) asString.
					MCMethodDefinition 
						className: className asString
						selector: 'version11MBFooTest:'
						category: 'cat'
						timeStamp: ''
						source: (self class sourceCodeAt: #version11MBFooTest:) asString.
					MCMethodDefinition 
						className: className asString
						selector: 'symbolicVersionStable10MBFooTest:'
						category: 'cat'
						timeStamp: ''
						source: (self class sourceCodeAt: #symbolicVersionStable10MBFooTest:) asString.
					MCMethodDefinition 
						className: className asString
						selector: 'version12MBFooTest:'
						category: 'cat'
						timeStamp: ''
						source: (self class sourceCodeAt: #version12MBFooTest:) asString.
				}.
	monticelloRepository basicStoreVersion: (MCVersion new
		setPackage:  (MCPackage new name: reference packageName)
		info: (versionInfo := MCVersionInfo
				name: reference name
				id: UUID new
				message: 'This is a mock version'
				date: Date today
				time: Time now
				author: reference author
				ancestors: ancestors)
		snapshot: (MCSnapshot fromDefinitions: definitionArray)
		dependencies: #()).
	^versionInfo
]

{ #category : #'command tests' }
MBMonticelloPackagesResource >> setUpConfigurationOfMBFooIssue117dkh2: ancestors [
	"self reset"
	| reference className definitionArray versionInfo  |
	reference := GoferVersionReference name: 'ConfigurationOfMBFooIssue117-dkh.2'.
	className := reference packageName asSymbol.
	definitionArray := {
					MCOrganizationDefinition categories: (Array with: reference packageName).
					MCClassDefinition name: className superclassName: #Object category: className instVarNames: #() comment: ''.
					MCMethodDefinition 
						className: className asString
						classIsMeta: true
						selector: 'isMetacelloConfig'
						category: 'cat'
						timeStamp: ''
						source: (self class class sourceCodeAt: #isMetacelloConfig) asString.
					MCMethodDefinition 
						className: className asString
						classIsMeta: true
						selector: 'project'
						category: 'cat'
						timeStamp: ''
						source: (self class class sourceCodeAt: #project) asString.
					MCMethodDefinition 
						className: className asString
						selector: 'project'
						category: 'cat'
						timeStamp: ''
						source: (self class sourceCodeAt: #project) asString.
					MCMethodDefinition 
						className: className asString
						selector: 'symbolicVersionDevelopment10MBFooTest:'
						category: 'cat'
						timeStamp: ''
						source: (self class sourceCodeAt: #symbolicVersionDevelopment10MBFooTest:) asString.
					MCMethodDefinition 
						className: className asString
						selector: 'baseline10MBFooTest:'
						category: 'cat'
						timeStamp: ''
						source: (self class sourceCodeAt: #baseline10MBFooTest:) asString.
					MCMethodDefinition 
						className: className asString
						selector: 'baseline20MBFooTest:'
						category: 'cat'
						timeStamp: ''
						source: (self class sourceCodeAt: #baseline20MBFooTest:) asString.
					MCMethodDefinition 
						className: className asString
						selector: 'version10MBFooTest:'
						category: 'cat'
						timeStamp: ''
						source: (self class sourceCodeAt: #version10MBFooTest:) asString.
					MCMethodDefinition 
						className: className asString
						selector: 'version11MBFooTest:'
						category: 'cat'
						timeStamp: ''
						source: (self class sourceCodeAt: #version11MBFooTest:) asString.
					MCMethodDefinition 
						className: className asString
						selector: 'symbolicVersionStable10MBFooTest:'
						category: 'cat'
						timeStamp: ''
						source: (self class sourceCodeAt: #symbolicVersionStable10MBFooTest:) asString.
					MCMethodDefinition 
						className: className asString
						selector: 'version12MBFooTest:'
						category: 'cat'
						timeStamp: ''
						source: (self class sourceCodeAt: #version12MBFooTest:) asString.
				}.
	monticelloRepository basicStoreVersion: (MCVersion new
		setPackage:  (MCPackage new name: reference packageName)
		info: (versionInfo := MCVersionInfo
				name: reference name
				id: UUID new
				message: 'This is a mock version'
				date: Date today
				time: Time now
				author: reference author
				ancestors: ancestors)
		snapshot: (MCSnapshot fromDefinitions: definitionArray)
		dependencies: #()).
	^versionInfo
]

{ #category : #'command tests' }
MBMonticelloPackagesResource >> setUpConfigurationOfMBFooIssue117dkh3: ancestors [
	"self reset"
	| reference className definitionArray versionInfo  |
	reference := GoferVersionReference name: 'ConfigurationOfMBFooIssue117-dkh.3'.
	className := reference packageName asSymbol.
	definitionArray := {
					MCOrganizationDefinition categories: (Array with: reference packageName).
					MCClassDefinition name: className superclassName: #Object category: className instVarNames: #() comment: ''.
					MCMethodDefinition 
						className: className asString
						classIsMeta: true
						selector: 'isMetacelloConfig'
						category: 'cat'
						timeStamp: ''
						source: (self class class sourceCodeAt: #isMetacelloConfig) asString.
					MCMethodDefinition 
						className: className asString
						classIsMeta: true
						selector: 'project'
						category: 'cat'
						timeStamp: ''
						source: (self class class sourceCodeAt: #project) asString.
					MCMethodDefinition 
						className: className asString
						selector: 'project'
						category: 'cat'
						timeStamp: ''
						source: (self class sourceCodeAt: #project) asString.
					MCMethodDefinition 
						className: className asString
						selector: 'symbolicVersionBleedingEdge20baselineMBFooTest:'
						category: 'cat'
						timeStamp: ''
						source: (self class sourceCodeAt: #symbolicVersionBleedingEdge20baselineMBFooTest:) asString.
					MCMethodDefinition 
						className: className asString
						selector: 'baseline10MBFooTest:'
						category: 'cat'
						timeStamp: ''
						source: (self class sourceCodeAt: #baseline10MBFooTest:) asString.
					MCMethodDefinition 
						className: className asString
						selector: 'symbolicVersionStable10MBFooTest:'
						category: 'cat'
						timeStamp: ''
						source: (self class sourceCodeAt: #symbolicVersionStable10MBFooTest:) asString.
					MCMethodDefinition 
						className: className asString
						selector: 'baseline20MBFooTest:'
						category: 'cat'
						timeStamp: ''
						source: (self class sourceCodeAt: #baseline20MBFooTest:) asString.
					MCMethodDefinition 
						className: className asString
						selector: 'baseline30MBFooTest:'
						category: 'cat'
						timeStamp: ''
						source: (self class sourceCodeAt: #baseline30MBFooTest:) asString.
					MCMethodDefinition 
						className: className asString
						selector: 'version10MBFooTest:'
						category: 'cat'
						timeStamp: ''
						source: (self class sourceCodeAt: #version10MBFooTest:) asString.
					MCMethodDefinition 
						className: className asString
						selector: 'version11MBFooTest:'
						category: 'cat'
						timeStamp: ''
						source: (self class sourceCodeAt: #version11MBFooTest:) asString.
					MCMethodDefinition 
						className: className asString
						selector: 'symbolicVersionStable10MBFooTest:'
						category: 'cat'
						timeStamp: ''
						source: (self class sourceCodeAt: #symbolicVersionStable10MBFooTest:) asString.
					MCMethodDefinition 
						className: className asString
						selector: 'version12MBFooTest:'
						category: 'cat'
						timeStamp: ''
						source: (self class sourceCodeAt: #version12MBFooTest:) asString.
				}.
	monticelloRepository basicStoreVersion: (MCVersion new
		setPackage:  (MCPackage new name: reference packageName)
		info: (versionInfo := MCVersionInfo
				name: reference name
				id: UUID new
				message: 'This is a mock version'
				date: Date today
				time: Time now
				author: reference author
				ancestors: ancestors)
		snapshot: (MCSnapshot fromDefinitions: definitionArray)
		dependencies: #()).
	^versionInfo
]

{ #category : #ConfigurationOfMBFooTest }
MBMonticelloPackagesResource >> setUpConfigurationOfMBFooTest [
	"self reset"  
	| versionInfo |
	versionInfo := self setUpConfigurationOfMBFooTest1.
	versionInfo := self setUpConfigurationOfMBFooTest2: { versionInfo }.
	versionInfo := self setUpConfigurationOfMBFooTest3: { versionInfo }.

]

{ #category : #ConfigurationOfMBFooTest }
MBMonticelloPackagesResource >> setUpConfigurationOfMBFooTest1 [
	"self reset"
	| reference className definitionArray versionInfo  |
	reference := GoferVersionReference name: 'ConfigurationOfMBFooTests-dkh.1'.
	className := reference packageName asSymbol.
	definitionArray := {
					MCOrganizationDefinition categories: (Array with: reference packageName).
					MCClassDefinition name: className superclassName: #Object category: className instVarNames: #() comment: ''.
					MCMethodDefinition 
						className: className asString
						classIsMeta: true
						selector: 'isMetacelloConfig'
						category: 'cat'
						timeStamp: ''
						source: (self class class sourceCodeAt: #isMetacelloConfig) asString.
					MCMethodDefinition 
						className: className asString
						classIsMeta: true
						selector: 'project'
						category: 'cat'
						timeStamp: ''
						source: (self class class sourceCodeAt: #project) asString.
					MCMethodDefinition 
						className: className asString
						selector: 'project'
						category: 'cat'
						timeStamp: ''
						source: (self class sourceCodeAt: #project) asString.
					MCMethodDefinition 
						className: className asString
						selector: 'symbolicVersionDevelopmentNotDefinedMBFooTest:'
						category: 'cat'
						timeStamp: ''
						source: (self class sourceCodeAt: #symbolicVersionDevelopmentNotDefinedMBFooTest:) asString.
					MCMethodDefinition 
						className: className asString
						selector: 'baseline10MBFooTest:'
						category: 'cat'
						timeStamp: ''
						source: (self class sourceCodeAt: #baseline10MBFooTest:) asString.
					MCMethodDefinition 
						className: className asString
						selector: 'version10MBFooTest:'
						category: 'cat'
						timeStamp: ''
						source: (self class sourceCodeAt: #version10MBFooTest:) asString.
					MCMethodDefinition 
						className: className asString
						selector: 'symbolicVersionStable10MBFooTest:'
						category: 'cat'
						timeStamp: ''
						source: (self class sourceCodeAt: #symbolicVersionStable10MBFooTest:) asString.
				}.
	monticelloRepository basicStoreVersion: (MCVersion new
		setPackage:  (MCPackage new name: reference packageName)
		info: (versionInfo := MCVersionInfo
				name: reference name
				id: UUID new
				message: 'This is a mock version'
				date: Date today
				time: Time now
				author: reference author
				ancestors: #())
		snapshot: (MCSnapshot fromDefinitions: definitionArray)
		dependencies: #()).
	^versionInfo
]

{ #category : #ConfigurationOfMBFooTest }
MBMonticelloPackagesResource >> setUpConfigurationOfMBFooTest2: ancestors [
	"self reset"
	| reference className definitionArray versionInfo  |
	reference := GoferVersionReference name: 'ConfigurationOfMBFooTests-dkh.2'.
	className := reference packageName asSymbol.
	definitionArray := {
					MCOrganizationDefinition categories: (Array with: reference packageName).
					MCClassDefinition name: className superclassName: #Object category: className instVarNames: #() comment: ''.
					MCMethodDefinition 
						className: className asString
						classIsMeta: true
						selector: 'isMetacelloConfig'
						category: 'cat'
						timeStamp: ''
						source: (self class class sourceCodeAt: #isMetacelloConfig) asString.
					MCMethodDefinition 
						className: className asString
						classIsMeta: true
						selector: 'project'
						category: 'cat'
						timeStamp: ''
						source: (self class class sourceCodeAt: #project) asString.
					MCMethodDefinition 
						className: className asString
						selector: 'project'
						category: 'cat'
						timeStamp: ''
						source: (self class sourceCodeAt: #project) asString.
					MCMethodDefinition 
						className: className asString
						selector: 'symbolicVersionDevelopmentNotDefinedMBFooTest:'
						category: 'cat'
						timeStamp: ''
						source: (self class sourceCodeAt: #symbolicVersionDevelopmentNotDefinedMBFooTest:) asString.
					MCMethodDefinition 
						className: className asString
						selector: 'baseline10MBFooTest:'
						category: 'cat'
						timeStamp: ''
						source: (self class sourceCodeAt: #baseline10MBFooTest:) asString.
					MCMethodDefinition 
						className: className asString
						selector: 'version10MBFooTest:'
						category: 'cat'
						timeStamp: ''
						source: (self class sourceCodeAt: #version10MBFooTest:) asString.
					MCMethodDefinition 
						className: className asString
						selector: 'version11MBFooTest:'
						category: 'cat'
						timeStamp: ''
						source: (self class sourceCodeAt: #version11MBFooTest:) asString.
					MCMethodDefinition 
						className: className asString
						selector: 'symbolicVersionStable11MBFooTest:'
						category: 'cat'
						timeStamp: ''
						source: (self class sourceCodeAt: #symbolicVersionStable11MBFooTest:) asString.
				}.
	monticelloRepository basicStoreVersion: (MCVersion new
		setPackage:  (MCPackage new name: reference packageName)
		info: (versionInfo := MCVersionInfo
				name: reference name
				id: UUID new
				message: 'This is a mock version'
				date: Date today
				time: Time now
				author: reference author
				ancestors: ancestors)
		snapshot: (MCSnapshot fromDefinitions: definitionArray)
		dependencies: #()).
	^versionInfo
]

{ #category : #ConfigurationOfMBFooTest }
MBMonticelloPackagesResource >> setUpConfigurationOfMBFooTest3: ancestors [
	"self reset"
	| reference className definitionArray versionInfo  |
	reference := GoferVersionReference name: 'ConfigurationOfMBFooTests-dkh.3'.
	className := reference packageName asSymbol.
	definitionArray := {
					MCOrganizationDefinition categories: (Array with: reference packageName).
					MCClassDefinition name: className superclassName: #Object category: className instVarNames: #() comment: ''.
					MCMethodDefinition 
						className: className asString
						classIsMeta: true
						selector: 'isMetacelloConfig'
						category: 'cat'
						timeStamp: ''
						source: (self class class sourceCodeAt: #isMetacelloConfig) asString.
					MCMethodDefinition 
						className: className asString
						classIsMeta: true
						selector: 'project'
						category: 'cat'
						timeStamp: ''
						source: (self class class sourceCodeAt: #project) asString.
					MCMethodDefinition 
						className: className asString
						selector: 'project'
						category: 'cat'
						timeStamp: ''
						source: (self class sourceCodeAt: #project) asString.
					MCMethodDefinition 
						className: className asString
						selector: 'symbolicVersionDevelopmentNotDefinedMBFooTest:'
						category: 'cat'
						timeStamp: ''
						source: (self class sourceCodeAt: #symbolicVersionDevelopmentNotDefinedMBFooTest:) asString.
					MCMethodDefinition 
						className: className asString
						selector: 'baseline10MBFooTest:'
						category: 'cat'
						timeStamp: ''
						source: (self class sourceCodeAt: #baseline10MBFooTest:) asString.
					MCMethodDefinition 
						className: className asString
						selector: 'version10MBFooTest:'
						category: 'cat'
						timeStamp: ''
						source: (self class sourceCodeAt: #version10MBFooTest:) asString.
					MCMethodDefinition 
						className: className asString
						selector: 'version11MBFooTest:'
						category: 'cat'
						timeStamp: ''
						source: (self class sourceCodeAt: #version11MBFooTest:) asString.
					MCMethodDefinition 
						className: className asString
						selector: 'symbolicVersionStable10MBFooTest:'
						category: 'cat'
						timeStamp: ''
						source: (self class sourceCodeAt: #symbolicVersionStable10MBFooTest:) asString.
					MCMethodDefinition 
						className: className asString
						selector: 'version12MBFooTest:'
						category: 'cat'
						timeStamp: ''
						source: (self class sourceCodeAt: #version12MBFooTest:) asString.
				}.
	monticelloRepository basicStoreVersion: (MCVersion new
		setPackage:  (MCPackage new name: reference packageName)
		info: (versionInfo := MCVersionInfo
				name: reference name
				id: UUID new
				message: 'This is a mock version'
				date: Date today
				time: Time now
				author: reference author
				ancestors: ancestors)
		snapshot: (MCSnapshot fromDefinitions: definitionArray)
		dependencies: #()).
	^versionInfo
]

{ #category : #running }
MBMonticelloPackagesResource >> setUpMonticelloRepository [
	"This method builds a fake repository with the version references from #buildReferences."
	"self reset"

	monticelloRepository := MCDictionaryRepository new.

]

{ #category : #running }
MBMonticelloPackagesResource >> setUpPackageClassDefs: classDefArray methodDefs: methodDefArray named: packageName ancestors: ancestors [
	"self reset"
	
	| definitions superclassName className ivNames cvNames civNames versionInfo methodSelector methodSource basePackageName authorName |
	definitions := OrderedCollection new.
	basePackageName := packageName copyUpToLast: $-.
	authorName := (packageName copyAfterLast: $-) copyUpTo: $..
	1 to: classDefArray size by: 5 do: [:index |
		superclassName := (classDefArray at: index) asSymbol.
		className := (classDefArray at: index + 1) asSymbol.
		ivNames := (classDefArray at: index + 2).
		cvNames := (classDefArray at: index + 3).
		civNames := (classDefArray at: index + 4).
		
		definitions add: (MCOrganizationDefinition categories: (Array with: basePackageName)).
		definitions add: (MCClassDefinition 
			name: className 
			superclassName: superclassName 
			category: basePackageName 
			instVarNames: ivNames 
			classVarNames: cvNames
			poolDictionaryNames: #()
			classInstVarNames: civNames
			comment: '').
		(cvNames, ivNames) do: [:ivName | 
			definitions addAll: {
					MCMethodDefinition 
						className: className asString
						selector: ivName asSymbol
						category: 'cat'
						timeStamp: ''
						source: ivName, ' ^', ivName.
					MCMethodDefinition 
						className: className asString
						selector: (ivName, ':') asSymbol
						category: 'cat'
						timeStamp: ''
						source: ivName, ':  anObject ', ivName, ' := anObject'.
				} ].
		civNames do: [:ivName | 
			definitions addAll: {
					MCMethodDefinition 
						className: className asString
						selector: ivName asSymbol
						category: 'cat'
						timeStamp: ''
						source: ivName, ' ^self class ', ivName.
					MCMethodDefinition 
						className: className asString
						selector: (ivName, ':') asSymbol
						category: 'cat'
						timeStamp: ''
						source: ivName, ':  anObject self class ', ivName, ': anObject'.
					MCMethodDefinition 
						className: className asString
						classIsMeta: true
						selector: ivName asSymbol
						category: 'cat'
						timeStamp: ''
						source: ivName, ' ^', ivName.
					MCMethodDefinition 
						className: className asString
						classIsMeta: true
						selector: (ivName, ':') asSymbol
						category: 'cat'
						timeStamp: ''
						source: ivName, ':  anObject ', ivName, ' := anObject'.
				} ].
		ivNames do: [:ivName|
			definitions add: 
					(MCMethodDefinition 
						className: className asString
						selector: #sampleValue
						category: 'cat'
						timeStamp: ''
						source: 'sampleValue ^#''', ivName, '''') ].
		].
	1 to: methodDefArray size by: 3 do: [:index |
		className := (methodDefArray at: index) asSymbol.
		methodSelector := (methodDefArray at: index + 1) asSymbol.
		methodSource := (methodDefArray at: index + 2).
		definitions add: 
					(MCMethodDefinition 
						className: className asString
						selector: methodSelector
						category: ('*', packageName) asString
						timeStamp: ''
						source: methodSource).
		].
	monticelloRepository basicStoreVersion: (MCVersion new
		setPackage:  (MCPackage new name: basePackageName)
		info: (versionInfo := MCVersionInfo
				name: packageName
				id: UUID new
				message: 'This is a mock version'
				date: Date today
				time: Time now
				author: authorName
				ancestors: ancestors)
		snapshot: (MCSnapshot fromDefinitions: definitions asArray)
		dependencies: #()).
	^versionInfo

]

{ #category : #running }
MBMonticelloPackagesResource >> setUpPackageClassDefs: classDefArray named: pName ancestors: ancestors [
	"self reset"
	
	^self setUpPackageClassDefs: classDefArray methodDefs: #() named: pName ancestors: ancestors

]

{ #category : #'command tests' }
MBMonticelloPackagesResource >> setUpPackagesMBCommandFooTests [
	"self reset"
	
	self
		setUpPackageClassDefs: { #Object. #MBFooCommandTestA. #(). #(). #(). }
		methodDefs: #()
		named: 'MBFooCommandTestA-dkh.1'
		ancestors: #().
	self
		setUpPackageClassDefs: { #Object. #MBFooCommandTestB. #(). #(). #(). }
		methodDefs: #()
		named: 'MBFooCommandTestB-dkh.1'
		ancestors: #().

]

{ #category : #ConfigurationOfMBFooTest }
MBMonticelloPackagesResource >> setUpPackagesMBFooTests [
	"self reset"
	
	| versionInfo |
	versionInfo := self
		setUpPackageClassDefs: { #Object. #MBFooTestA. #(). #(). #(). }
		methodDefs: #()
		named: 'MBFooTests-dkh.1'
		ancestors: #().
	versionInfo := self 
		setUpPackageClassDefs: { #Object. #MBFooTestA. #(). #(). #(). }
		methodDefs: #()
		named: 'MBFooTests-dkh.2'
		ancestors: {versionInfo} .
	versionInfo := self 
		setUpPackageClassDefs: { #Object. #MBFooTestA. #(). #(). #(). }
		methodDefs: #(MBFooTestA foo 'foo ^self')
		named: 'MBFooTests-dkh.3'
		ancestors: {versionInfo} .
	versionInfo := self 
		setUpPackageClassDefs: { #Object. #MBFooTestA. #(). #(). #(). }
		methodDefs: #()
		named: 'MBFooTests-dkh.4'
		ancestors: {versionInfo} .

]

{ #category : #'command tests' }
MBMonticelloPackagesResource >> symbolicVersionBleedingEdge20baselineMBFooTest: spec [
	"self reset"
	<symbolicVersion: #'bleedingEdge'>

	spec for: #'common' version: '2.0-baseline'.
]

{ #category : #'command tests' }
MBMonticelloPackagesResource >> symbolicVersionDevelopment10MBFooTest: spec [
	"self reset"
	<symbolicVersion: #'development'>

	spec for: #'common' version: '1.0'.
]

{ #category : #ConfigurationOfMBFooTest }
MBMonticelloPackagesResource >> symbolicVersionDevelopmentNotDefinedMBFooTest: spec [
	"self reset"
	<symbolicVersion: #'development'>

	spec for: #'common' version: #'notDefined'.
]

{ #category : #ConfigurationOfMBFooTest }
MBMonticelloPackagesResource >> symbolicVersionStable10MBFooTest: spec [
	"self reset"
	<symbolicVersion: #'stable'>

	spec for: #'common' version: '1.0'.
]

{ #category : #ConfigurationOfMBFooTest }
MBMonticelloPackagesResource >> symbolicVersionStable11MBFooTest: spec [
	"self reset"
	<symbolicVersion: #'stable'>

	spec for: #'common' version: '1.1'.
]

{ #category : #ConfigurationOfMBFooTest }
MBMonticelloPackagesResource >> version10MBFooTest: spec [
	"self reset"
	<version: '1.0' imports: #('1.0-baseline')>

	spec for: #'common' do: [
		spec blessing: #release.
		spec package: 'MBFooTests' with: 'MBFooTests-dkh.1'
].

]

{ #category : #ConfigurationOfMBFooTest }
MBMonticelloPackagesResource >> version11MBFooTest: spec [
	"self reset"
	<version: '1.1' imports: #('1.0-baseline')>

	spec for: #'common' do: [
		spec blessing: #release.
		spec package: 'MBFooTests' with: 'MBFooTests-dkh.2'
].

]

{ #category : #ConfigurationOfMBFooTest }
MBMonticelloPackagesResource >> version12MBFooTest: spec [
	"self reset"
	<version: '1.2' imports: #('1.0-baseline')>

	spec for: #'common' do: [
		spec blessing: #release.
		spec package: 'MBFooTests' with: 'MBFooTests-dkh.3'
].

]

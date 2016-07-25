command tests
setUpConfigurationOfMBFooIssue117dkh3: ancestors
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
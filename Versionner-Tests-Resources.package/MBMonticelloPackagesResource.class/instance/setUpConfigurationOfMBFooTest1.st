ConfigurationOfMBFooTest
setUpConfigurationOfMBFooTest1
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
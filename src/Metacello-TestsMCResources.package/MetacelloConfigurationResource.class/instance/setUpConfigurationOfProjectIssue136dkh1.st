Issue 136
setUpConfigurationOfProjectIssue136dkh1
	"self reset"
	
	| reference className definitionArray versionInfo |
	reference := GoferVersionReference name: 'MetacelloTestConfigurationOfProjectIssue136-dkh.1'.
	className := reference packageName asSymbol.
	definitionArray := {
					MCOrganizationDefinition categories: (Array with: className).
					MCClassDefinition name: className superclassName: #Object category: className instVarNames: #() comment: ''.
					MCMethodDefinition 
						className: className asString
						classIsMeta: true
						selector: 'project'
						category: 'cat'
						timeStamp: ''
						source: self projectClassMethodSource.
					MCMethodDefinition 
						className: className asString
						selector: 'project'
						category: 'cat'
						timeStamp: ''
						source: self projectMethodSource.
					MCMethodDefinition 
						className: className asString
						selector: 'baselineVersion10Issue136:'
						category: 'cat'
						timeStamp: ''
						source: (self class sourceCodeAt: #baselineVersion10Issue136:) asString.
					MCMethodDefinition 
						className: className asString
						selector: 'version10Issue136:'
						category: 'cat'
						timeStamp: ''
						source: (self class sourceCodeAt: #version10Issue136:) asString.
				}.
	monticelloRepository basicStoreVersion: (MCVersion new
		setPackage:  (MetacelloTestsMCPackage new name: reference packageName)
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
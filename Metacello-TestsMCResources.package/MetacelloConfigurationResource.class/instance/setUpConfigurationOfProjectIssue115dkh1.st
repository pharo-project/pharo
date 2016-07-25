Issue 115
setUpConfigurationOfProjectIssue115dkh1
	"self reset"
	
	| reference className definitionArray versionInfo |
	reference := GoferVersionReference name: 'MetacelloTestConfigurationOfProjectIssue115-dkh.1'.
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
						selector: 'baselineVersion10Issue115:'
						category: 'cat'
						timeStamp: ''
						source: (self class sourceCodeAt: #baselineVersion10Issue115:) asString.
					MCMethodDefinition 
						className: className asString
						selector: 'version10Issue115:'
						category: 'cat'
						timeStamp: ''
						source: (self class sourceCodeAt: #version10Issue115:) asString.
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
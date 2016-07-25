ConfigurationOfSymbolic
setUpConfigurationOfSymbolic
	"self reset"
	
	| reference className definitionArray |
	reference := GoferVersionReference name: 'MetacelloTestConfigurationOfSymbolic-dkh.1'.
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
						selector: 'stableVersionD:'
						category: 'cat'
						timeStamp: ''
						source: self stableVersionDMethodSourceSymbolic.
					MCMethodDefinition 
						className: className asString
						selector: 'version40Symbolic:'
						category: 'cat'
						timeStamp: ''
						source: self version40SymbolicMethodSourceSymbolic.
					MCMethodDefinition 
						className: className asString
						selector: 'version41Symbolic:'
						category: 'cat'
						timeStamp: ''
						source: self version41SymbolicMethodSourceSymbolic.
					MCMethodDefinition 
						className: className asString
						selector: 'version42Symbolic:'
						category: 'cat'
						timeStamp: ''
						source: self version42SymbolicMethodSourceSymbolic.
					MCMethodDefinition 
						className: className asString
						selector: 'version43Symbolic:'
						category: 'cat'
						timeStamp: ''
						source: self version43SymbolicMethodSourceSymbolic.
				}.
	monticelloRepository basicStoreVersion: (MCVersion new
		setPackage:  (MetacelloTestsMCPackage new name: reference packageName)
		info: (MCVersionInfo
				name: reference name
				id: UUID new
				message: 'This is a mock version'
				date: Date today
				time: Time now
				author: reference author
				ancestors: #())
		snapshot: (MCSnapshot fromDefinitions: definitionArray)
		dependencies: #()) 

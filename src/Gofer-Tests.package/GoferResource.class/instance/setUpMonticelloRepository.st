running
setUpMonticelloRepository
	"This method builds a fake repository with the version references from #buildReferences."
	| packages |
	
	packages := Dictionary new.
	monticelloRepository := MCDictionaryRepository new.
	versionReferences do: [ :reference |
		monticelloRepository basicStoreVersion: (MCVersion new
			setPackage:  (packages 
				at: reference packageName 
				ifAbsentPut: [ MetacelloTestsMCPackage new name: reference packageName ])
			info: (MCVersionInfo
				name: reference name
				id: UUID new
				message: 'This is a mock version'
				date: Date today
				time: Time now
				author: reference author
				ancestors: #())
			snapshot: (MCSnapshot fromDefinitions: (Array
				with: (MCOrganizationDefinition categories: (Array with: reference packageName asSymbol))
				with: (MCClassDefinition name: (reference packageName copyWithout: $-) asSymbol superclassName: #Object category: reference packageName asSymbol instVarNames: #() comment: '')))
			dependencies: #()) ].
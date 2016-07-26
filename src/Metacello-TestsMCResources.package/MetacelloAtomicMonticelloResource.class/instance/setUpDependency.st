running
setUpDependency
	"self reset"

	| reference |
	reference := GoferVersionReference name: 'GoferUmbrella-dkh.4'.
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
		snapshot: (MCSnapshot fromDefinitions: (Array
				with: (MCOrganizationDefinition categories: (Array with: reference packageName asSymbol))
				with: (MCClassDefinition name: (reference packageName copyWithout: $-) asSymbol superclassName: #Object category: reference packageName asSymbol instVarNames: #() comment: '')))
		dependencies: (Array 
				with: (MCVersionDependency 
						package: (MetacelloTestsMCPackage new name: 'GoferBarDependency') 
						info: (monticelloRepository versionInfoFromVersionNamed: 'GoferBarDependency-lr.30')) 
				with: (MCVersionDependency 
						package: (MetacelloTestsMCPackage new name: 'GoferFooDependency') 
						info: (monticelloRepository versionInfoFromVersionNamed: 'GoferFooDependency-lr.30'))))
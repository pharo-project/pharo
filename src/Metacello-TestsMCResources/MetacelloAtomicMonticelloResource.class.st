Class {
	#name : #MetacelloAtomicMonticelloResource,
	#superclass : #TestResource,
	#instVars : [
		'versionReferences',
		'monticelloRepository'
	],
	#category : #'Metacello-TestsMCResources'
}

{ #category : #accessing }
MetacelloAtomicMonticelloResource >> monticelloRepository [
	^ monticelloRepository
]

{ #category : #running }
MetacelloAtomicMonticelloResource >> setUp [
	super setUp.
	self
		setUpVersionReferences;
		setUpMonticelloRepository;
		setUpDependency;
		setUpNewerDependency
]

{ #category : #running }
MetacelloAtomicMonticelloResource >> setUpDependency [
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
]

{ #category : #running }
MetacelloAtomicMonticelloResource >> setUpMonticelloRepository [
	"This method builds a fake repository with the version references from #buildReferences."

	monticelloRepository := MCDictionaryRepository new.
	versionReferences do: [ :assoc | | reference superclassName |
		reference := assoc key.
		superclassName := assoc value.
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
				with: (MCClassDefinition name: (reference packageName copyWithout: $-) asSymbol superclassName: superclassName category: reference packageName asSymbol instVarNames: #() comment: '')))
			dependencies: #()) ]
]

{ #category : #running }
MetacelloAtomicMonticelloResource >> setUpNewerDependency [
	"self reset"

	| reference |
	reference := GoferVersionReference name: 'GoferUmbrella-lr.5'.
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
						info: (monticelloRepository versionInfoFromVersionNamed: 'GoferBarDependency-lr.35')) 
				with: (MCVersionDependency 
						package: (MetacelloTestsMCPackage new name: 'GoferFooDependency') 
						info: (monticelloRepository versionInfoFromVersionNamed: 'GoferFooDependency-lr.35'))))
]

{ #category : #running }
MetacelloAtomicMonticelloResource >> setUpVersionReferences [
	"self reset"

	versionReferences := OrderedCollection new.
	versionReferences
		add: ((GoferVersionReference name: 'GoferFan-dkh.1') -> #GoferFar);
		add: ((GoferVersionReference name: 'GoferBar-dkh.1') -> #Object);
		add: ((GoferVersionReference name: 'GoferFoo-dkh.4') -> #GoferBeau);
		add: ((GoferVersionReference name: 'GoferFoo-dkh.5') -> #GoferBeau);
		add: ((GoferVersionReference name: 'GoferBeau-dkh.15') -> #GoferBar);
		add: ((GoferVersionReference name: 'GoferFaux-tg.30') -> #Object);
		add: ((GoferVersionReference name: 'GoferFar-dkh.4') -> #GoferFoo);
		add: ((GoferVersionReference name: 'GoferBarDependency-dkh.4') -> #Object);
		add: ((GoferVersionReference name: 'GoferFooDependency-lr.4') -> #Object);
		add: ((GoferVersionReference name: 'GoferBarDependency-lr.30') -> #Object);
		add: ((GoferVersionReference name: 'GoferFooDependency-lr.30') -> #Object);
		add: ((GoferVersionReference name: 'GoferBarDependency-lr.35') -> #Object);
		add: ((GoferVersionReference name: 'GoferFooDependency-lr.35') -> #Object);
		yourself
	
]

{ #category : #accessing }
MetacelloAtomicMonticelloResource >> versionReferences [
	^ versionReferences
]

Class {
	#name : #MetacelloAlternateResource,
	#superclass : #TestResource,
	#instVars : [
		'versionReferences',
		'monticelloRepository'
	],
	#category : #Metacello-TestsMCResources
}

{ #category : #accessing }
MetacelloAlternateResource >> monticelloRepository [
	^ monticelloRepository
]

{ #category : #running }
MetacelloAlternateResource >> setUp [
	super setUp.
	self
		setUpVersionReferences;
		setUpMonticelloRepository
]

{ #category : #running }
MetacelloAlternateResource >> setUpMonticelloRepository [
	"This method builds a fake repository with the version references from #buildReferences."

	monticelloRepository := MCDictionaryRepository new.
	versionReferences do: [ :reference |
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
				with: (MCClassDefinition name: reference packageName asSymbol superclassName: #Object category: reference packageName asSymbol instVarNames: #() comment: '')))
			dependencies: #()) ]
]

{ #category : #running }
MetacelloAlternateResource >> setUpVersionReferences [
	"self reset"

	versionReferences := OrderedCollection new.
	versionReferences
		add: (GoferVersionReference name: 'GoferBar-dkh.24');
		add: (GoferVersionReference name: 'GoferBar-dkh.25');
		add: (GoferVersionReference name: 'GoferFoo-dkh.75');
		add: (GoferVersionReference name: 'GoferFoo-dkh.104')
]

{ #category : #accessing }
MetacelloAlternateResource >> versionReferences [
	^ versionReferences
]

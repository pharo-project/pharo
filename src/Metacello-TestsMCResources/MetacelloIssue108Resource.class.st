Class {
	#name : #MetacelloIssue108Resource,
	#superclass : #TestResource,
	#instVars : [
		'versionReferences',
		'monticelloRepository'
	],
	#category : #'Metacello-TestsMCResources'
}

{ #category : #accessing }
MetacelloIssue108Resource >> monticelloRepository [
	^ monticelloRepository
]

{ #category : #running }
MetacelloIssue108Resource >> setUp [
  "https://code.google.com/p/metacello/issues/detail?id=108"

  super setUp.
  self
    setUpVersionReferences;
    setUpMonticelloRepository
]

{ #category : #running }
MetacelloIssue108Resource >> setUpMonticelloRepository [
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
				with: (MCClassDefinition name: (reference packageName copyWithout: $-) asSymbol superclassName: #Object category: reference packageName asSymbol instVarNames: #() comment: '')))
			dependencies: #()) ]
]

{ #category : #running }
MetacelloIssue108Resource >> setUpVersionReferences [
	"self reset"

	versionReferences := OrderedCollection new.
	versionReferences
		add: (GoferVersionReference name: 'GoferBar.gemstone-dkh.68');
		add: (GoferVersionReference name: 'GoferBar.gemstone-dkh.69');
		add: (GoferVersionReference name: 'GoferBar.gemstone-dkh.70');
		add: (GoferVersionReference name: 'GoferBar-dkh.68');
		add: (GoferVersionReference name: 'GoferBar-dkh.69');
		add: (GoferVersionReference name: 'GoferBar-dkh.70');
		yourself
	
]

{ #category : #accessing }
MetacelloIssue108Resource >> versionReferences [
	^ versionReferences
]

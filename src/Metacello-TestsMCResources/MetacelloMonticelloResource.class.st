Class {
	#name : #MetacelloMonticelloResource,
	#superclass : #TestResource,
	#instVars : [
		'versionReferences',
		'monticelloRepository'
	],
	#category : 'Metacello-TestsMCResources'
}

{ #category : #accessing }
MetacelloMonticelloResource >> monticelloRepository [
	^ monticelloRepository
]

{ #category : #running }
MetacelloMonticelloResource >> setUp [
	super setUp.
	self
		setUpVersionReferences;
		setUpMonticelloRepository
]

{ #category : #running }
MetacelloMonticelloResource >> setUpMonticelloRepository [
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
MetacelloMonticelloResource >> setUpVersionReferences [
  "self reset"

  versionReferences := OrderedCollection new.
  versionReferences
    add: (GoferVersionReference name: 'GoferBar.branch-lr.1');
    add: (GoferVersionReference name: 'GoferBar.branch-lr.2');
    add: (GoferVersionReference name: 'GoferBar-jf.1');
    add: (GoferVersionReference name: 'GoferBar-lr.1');
    add: (GoferVersionReference name: 'GoferFoo-lr.1');
    add: (GoferVersionReference name: 'GoferFoo-lr.2');
    add: (GoferVersionReference name: 'GoferFoo-lr.4');
    add: (GoferVersionReference name: 'GoferBeau-dkh.15');
    add: (GoferVersionReference name: 'GoferBeau-dkh.25');
    add: (GoferVersionReference name: 'GoferBeau-dkh.53');
    add: (GoferVersionReference name: 'GoferBeau-dkh.54');
    add: (GoferVersionReference name: 'GoferBeau-dkh.55');
    add: (GoferVersionReference name: 'GoferBeau-dkh.56');
    add: (GoferVersionReference name: 'GoferFaux-tg.30');
    add: (GoferVersionReference name: 'GoferFaux-tg.31');
    add: (GoferVersionReference name: 'GoferFaux-tg.32');
    add: (GoferVersionReference name: 'GoferFaux-tg.33');
    add: (GoferVersionReference name: 'GoferFaux-tg.34');
    add: (GoferVersionReference name: 'GoferFaux-tg.35');
    add: (GoferVersionReference name: 'GeauxBar.branch-lr.2');
    add: (GoferVersionReference name: 'GeauxFoo-lr.1');
    add: (GoferVersionReference name: 'GeauxFaux-tg.32');
    add: (GoferVersionReference name: 'GeauxBeau-dkh.55');
    add: (GoferVersionReference name: 'GeauxBeau-dkh.56');
    add: (GoferVersionReference name: 'Issue339-JanKurs.1');
    add: (GoferVersionReference name: 'Issue339-JanKurs.11');
    add:
        (GoferVersionReference name: 'ConfigurationOfNautilusWithoutPackages-dkh.55');
    add:
        (GoferVersionReference name: 'ConfigurationOfNautilusWithoutPackages-dkh.56');
    add:
        (GoferVersionReference name: 'ConfigurationOfNautilusWithoutPackages-dkh.57');
    add: (GoferVersionReference name: 'ConfigurationOfNautilus-dkh.56');
    add: (GoferVersionReference name: 'ConfigurationOfGlamourSeaside-dkh.55');
    add: (GoferVersionReference name: 'ConfigurationOfGlamourSeaside-dkh.56');
    add: (GoferVersionReference name: 'ConfigurationOfGlamourSeaside-dkh.57');
    add: (GoferVersionReference name: 'ConfigurationOfGlamour-dkh.56');
    yourself
]

{ #category : #accessing }
MetacelloMonticelloResource >> versionReferences [
	^ versionReferences
]

"
I'm a provider for projects in the pharo catalog. Pharo projects are represented as CatalogProject instances. 

Usually  I fetch projects from http://catalog.pharo.org/catalog/json and create CatalogProject from such description. 

Use my default instance, I will cache the catalog data for 24 hours.

	CatalogProvider default projects.

"
Class {
	#name : #CatalogProvider,
	#superclass : #Object,
	#instVars : [
		'projects',
		'lastModified',
		'session'
	],
	#classVars : [
		'Default'
	],
	#category : #'Tool-Catalog-Base'
}

{ #category : #accessing }
CatalogProvider class >> default [
	"Return my default shared instance that caches the catalog data"
	
	^ Default ifNil: [ Default := self new ]
]

{ #category : #installation }
CatalogProvider class >> install: aProjectName version: aVersion [
	"
		self install: 'Mustache' version: #stable
	"
	^self new refresh projects 
		detect: [ :each | each name = aProjectName ]
		ifFound: [:proj | proj installVersion: aVersion ]
		ifNone: [ self error: 'Project ', aProjectName , ' not found' ]
]

{ #category : #accessing }
CatalogProvider class >> projectNamed: projectName [
	"Lookup and return a particular project in the catalog, else fail with NotFound"
	
	"self projectNamed: 'ZTimestamp'"
	
	^ self default projects 
		detect: [ :each | each name = projectName ]
		ifNone: [ NotFound signalFor: projectName in: self default ]
]

{ #category : #private }
CatalogProvider >> loadProjects [
	^ [ self retrieveProjects collect: [ :each | CatalogProject fromDictionary: each ] ]
		on: ConnectionTimedOut do: [ :e | projects "Answer old version" ] 
]

{ #category : #private }
CatalogProvider >> loadProjectsIfNeeded [
	self shouldLoadProjects ifFalse: [ ^ self ].
	projects := self loadProjects.
	session := Smalltalk session.
	lastModified := DateAndTime now
]

{ #category : #accessing }
CatalogProvider >> projects [
	"Return the list of projects known in the central catalog.
	This data is retrieved over the internet.
	I will cache this data for 24 hours."
	
	self loadProjectsIfNeeded.
	^ projects ifNil: [ #() ]
]

{ #category : #initialization }
CatalogProvider >> reset [
	"Reset my cached data so that I will reload it on next access"
	
	projects := lastModified := session := nil.
]

{ #category : #private }
CatalogProvider >> retrieveProjects [
	^ ZnClient new
		systemPolicy;
		beOneShot;
		timeout: 1;
		setAcceptEncodingGzip;
		accept: 'text/json';
		url: CatalogSettings catalogSourceURL;
		contentReader: [ :entity | STON fromString: entity contents ];
		get
]

{ #category : #private }
CatalogProvider >> shouldLoadProjects [
	^ session ~= Smalltalk session 
		or: [ lastModified notNil and: [ DateAndTime now - lastModified > 24 hours ] ]
]

Class {
	#name : 'BaselineOfPharo',
	#superclass : 'BaselineOf',
	#classVars : [
		'ExternalProjects'
	],
	#category : 'BaselineOfPharo',
	#package : 'BaselineOfPharo'
}

{ #category : 'external projects' }
BaselineOfPharo class >> beautifulComments [
	<externalProject>

	^ PharoExternalProject 
		newName: 'BeautifulComments' 
		owner: 'pillar-markup' 
		project: 'BeautifulComments' 
		version: 'Pharo12'
]

{ #category : 'repository urls' }
BaselineOfPharo class >> beautifulCommentsRepository [

	^ (self externalProjectNamed: 'BeautifulComments') repository
]

{ #category : 'private' }
BaselineOfPharo class >> declareExternalProjects [

	^ (Pragma allNamed: #externalProject in: self class)
		collect: [ :each | 
			each method 
				valueWithReceiver: self 
				arguments: #() ]
		as: Array
]

{ #category : 'external projects' }
BaselineOfPharo class >> documentBrowser [
	<externalProject>
	
	^ PharoExternalProject 
		newName: 'DocumentBrowser' 
		owner: 'pharo-spec' 
		project: 'NewTools-DocumentBrowser' 
		version: 'Pharo12'
]

{ #category : 'repository urls' }
BaselineOfPharo class >> documentBrowserRepository [

	^ (self externalProjectNamed: 'DocumentBrowser') repository
]

{ #category : 'external projects' }
BaselineOfPharo class >> externalProjectNamed: aName [

	^ self externalProjects 
		detect: [ :each | each name = aName ]
]

{ #category : 'external projects' }
BaselineOfPharo class >> externalProjects [

	^ ExternalProjects ifNil: [ 
		ExternalProjects := self declareExternalProjects ]
]

{ #category : 'external projects' }
BaselineOfPharo class >> iceberg [
	<externalProject>

	^ PharoExternalProject 
		newName: 'Iceberg' 
		owner: 'pharo-vcs' 
		project: 'iceberg' 
		version: 'dev-2.0' 
		sourceDir: nil
]

{ #category : 'repository urls' }
BaselineOfPharo class >> icebergRepository [

	^ (self externalProjectNamed: 'Iceberg') repository
]

{ #category : 'external projects' }
BaselineOfPharo class >> microdown [
	<externalProject>
	
	^ PharoExternalProject 
		newName: 'Microdown' 
		owner: 'pillar-markup' 
		project: 'Microdown' 
		version: 'Pharo12'
]

{ #category : 'repository urls' }
BaselineOfPharo class >> microdownRepository [

	^ (self externalProjectNamed: 'Microdown') repository
]

{ #category : 'external projects' }
BaselineOfPharo class >> newTools [
	<externalProject>

	^ PharoExternalProject 
		newName: 'NewTools' 
		owner: 'pharo-spec' 
		project: 'NewTools' 
		version: 'Pharo12'
]

{ #category : 'repository urls' }
BaselineOfPharo class >> newToolsRepository [

	^ (self externalProjectNamed: 'NewTools') repository
]

{ #category : 'external projects' }
BaselineOfPharo class >> roassal [
	<externalProject>	

	^ PharoExternalProject 
		newName: 'Roassal3' 
		owner: 'ObjectProfile' 
		project: 'Roassal3' 
		version: 'v1.03'
]

{ #category : 'repository urls' }
BaselineOfPharo class >> roassal3Repository [

	^ (self externalProjectNamed: 'Roassal3') repository
]

{ #category : 'external projects' }
BaselineOfPharo class >> spec [
	<externalProject>

	^ PharoExternalProject 
		newName: 'Spec2' 
		owner: 'pharo-spec' 
		project:'Spec' 
		version: 'Pharo12'
]

{ #category : 'repository urls' }
BaselineOfPharo class >> specRepository [

	^ (self externalProjectNamed: 'Spec2') repository
]

{ #category : 'external projects' }
BaselineOfPharo class >> welcomeBrowser [
	<externalProject>
	
	^ PharoExternalProject 
		newName: 'WelcomeBrowser' 
		owner: 'pharo-spec' 
		project: 'NewTools-WelcomeBrowser' 
		version: 'Pharo12'
]

{ #category : 'repository urls' }
BaselineOfPharo class >> welcomeBrowserRepository [

	^ (self externalProjectNamed: 'WelcomeBrowser') repository
]

{ #category : 'baselines' }
BaselineOfPharo >> baseline: spec [
	<baseline>
	| repository |
	repository := self packageRepositoryURL.

	spec for: #common do: [
		spec postLoadDoIt: #'postload:package:'.
		spec baseline: 'BaseLibraries' with: [spec repository: repository].
		
		spec baseline: 'IDE' with: [spec repository: repository].
		spec baseline: 'Calypso' with: [ 
			spec repository: repository; loads: #('IcebergSupport'). ].
		
		spec package: 'Deprecated12'.
	]
]

{ #category : 'baseline' }
BaselineOfPharo >> postload: loader package: packageSpec [
	"If we added a github token for the build, we remove it."

	Smalltalk os environment at: #GITHUB_TOKEN ifPresent: [ :token |
		| credential |
		credential := (Smalltalk classNamed: #IceCredentialStore) current plaintextCredentialForHostname: 'github.com'.
		credential password = token ifTrue: [ (Smalltalk classNamed: #IceCredentialStore) current removePlainTextCredential: credential ].
		'Removing credential.' traceCr ].

	"Load the WelcomeBrowser as last step"
		(self class environment classNamed: #StWelcomeBrowser)
			ifNotNil: [ :aClass | aClass openForRelease ]
]

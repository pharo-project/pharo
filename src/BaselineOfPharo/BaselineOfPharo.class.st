Class {
	#name : #BaselineOfPharo,
	#superclass : #BaselineOf,
	#classVars : [
		'ExternalProjects'
	],
	#category : #BaselineOfPharo
}

{ #category : #'external projects' }
BaselineOfPharo class >> beautifulComments [
	<externalProject>

	^ PharoExternalProject 
		newName: 'BeautifulComments' 
		owner: 'pillar-markup' 
		project: 'BeautifulComments' 
		version: 'v1.0.1'
]

{ #category : #'repository urls' }
BaselineOfPharo class >> beautifulCommentsRepository [

	^ (self externalProjectNamed: 'BeautifulComments') repository
]

{ #category : #private }
BaselineOfPharo class >> declareExternalProjects [

	^ (Pragma allNamed: #externalProject in: self class)
		collect: [ :each | 
			each method 
				valueWithReceiver: self 
				arguments: #() ]
		as: Array
]

{ #category : #'external projects' }
BaselineOfPharo class >> documentBrowser [
	<externalProject>
	
	^ PharoExternalProject 
		newName: 'DocumentBrowser' 
		owner: 'pharo-spec' 
		project: 'NewTools-DocumentBrowser' 
		version: 'v1.0.3'
]

{ #category : #'repository urls' }
BaselineOfPharo class >> documentBrowserRepository [

	^ (self externalProjectNamed: 'DocumentBrowser') repository
]

{ #category : #'external projects' }
BaselineOfPharo class >> externalProjectNamed: aName [

	^ self externalProjects 
		detect: [ :each | each name = aName ]
]

{ #category : #'external projects' }
BaselineOfPharo class >> externalProjects [

	^ ExternalProjects ifNil: [ 
		ExternalProjects := self declareExternalProjects ]
]

{ #category : #'external projects' }
BaselineOfPharo class >> iceberg [
	<externalProject>

	^ PharoExternalProject 
		newName: 'Iceberg' 
		owner: 'pharo-vcs' 
		project: 'iceberg' 
		version: 'v2.2.2' 
		sourceDir: nil
]

{ #category : #'repository urls' }
BaselineOfPharo class >> icebergRepository [

	^ (self externalProjectNamed: 'Iceberg') repository
]

{ #category : #'external projects' }
BaselineOfPharo class >> microdown [
	<externalProject>
	
	^ PharoExternalProject 
		newName: 'Microdown' 
		owner: 'pillar-markup' 
		project: 'Microdown' 
		version: 'v2.4.1'
]

{ #category : #'repository urls' }
BaselineOfPharo class >> microdownRepository [

	^ (self externalProjectNamed: 'Microdown') repository
]

{ #category : #'external projects' }
BaselineOfPharo class >> newTools [
	<externalProject>

	^ PharoExternalProject 
		newName: 'NewTools' 
		owner: 'pharo-spec' 
		project: 'NewTools' 
		version: 'v0.8.4'
]

{ #category : #'repository urls' }
BaselineOfPharo class >> newToolsRepository [

	^ (self externalProjectNamed: 'NewTools') repository
]

{ #category : #'external projects' }
BaselineOfPharo class >> roassal [
	<externalProject>	
		
	^ PharoExternalProject 
		newName: 'Roassal3' 
		owner: 'ObjectProfile' 
		project: 'Roassal3' 
		version: 'v0.9.9m'
]

{ #category : #'repository urls' }
BaselineOfPharo class >> roassal3Repository [

	^ (self externalProjectNamed: 'Roassal3') repository
]

{ #category : #'external projects' }
BaselineOfPharo class >> spec [
	<externalProject>

	^ PharoExternalProject 
		newName: 'Spec2' 
		owner: 'pharo-spec' 
		project:'Spec' 
		version: 'v1.2.5'
]

{ #category : #'repository urls' }
BaselineOfPharo class >> specRepository [

	^ (self externalProjectNamed: 'Spec2') repository
]

{ #category : #'external projects' }
BaselineOfPharo class >> welcomeBrowser [
	<externalProject>
	
	^ PharoExternalProject 
		newName: 'WelcomeBrowser' 
		owner: 'pharo-spec' 
		project: 'NewTools-WelcomeBrowser' 
		version: 'v1.0.2'
]

{ #category : #'repository urls' }
BaselineOfPharo class >> welcomeBrowserRepository [

	^ (self externalProjectNamed: 'WelcomeBrowser') repository
]

{ #category : #baselines }
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
		
		spec package: 'Deprecated11'.
		
		spec package: 'FluidClassBuilder'.
		spec package: 'FluidClassBuilder-Tests' with: [ spec requires: 'FluidClassBuilder' ].
	]
]

{ #category : #baseline }
BaselineOfPharo >> postload: loader package: packageSpec [

	"Load the WelcomeBrowser as last step"
	(self class environment classNamed: #StWelcomeBrowser)
		ifNotNil: [ :aClass | aClass openForRelease ].

	"Here we will do something for the release announce"

	"If we added a github token for the build, we remove it."
	Smalltalk os environment at: #GITHUB_TOKEN ifPresent: [ :token |
		| credential |
		credential := (Smalltalk classNamed: #IceCredentialStore) current plaintextCredentialForHostname: 'github.com'.

		credential password = token ifTrue: [ (Smalltalk classNamed: #IceCredentialStore) current removePlainTextCredential: credential ].

		'Removing credential.' traceCr ]
]

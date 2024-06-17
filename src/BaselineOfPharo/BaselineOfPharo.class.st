"
This is a primary baseline to load Pharo base libraries, the IDE as well as projects that are managed in own repository (so called ""external projects"") and other
"
Class {
	#name : 'BaselineOfPharo',
	#superclass : 'BaselineOf',
	#classVars : [
		'ExternalProjects'
	],
	#category : 'BaselineOfPharo-Baseline',
	#package : 'BaselineOfPharo',
	#tag : 'Baseline'
}

{ #category : 'private' }
BaselineOfPharo class >> declareExternalProjects [

	^ (Pragma allNamed: #externalProject in: self class)
			collect: [ :each | each method valueWithReceiver: self ]
			as: Array
]

{ #category : 'accessing - external projects' }
BaselineOfPharo class >> documentBrowser [
	<externalProject>
	
	^ PharoExternalProject 
		newName: 'DocumentBrowser' 
		owner: 'pharo-spec' 
		project: 'NewTools-DocumentBrowser' 
		version: 'Pharo13'
]

{ #category : 'repository urls' }
BaselineOfPharo class >> documentBrowserRepository [

	^ (self externalProjectNamed: 'DocumentBrowser') repository
]

{ #category : 'accessing - external projects' }
BaselineOfPharo class >> externalProjectNamed: aName [

	^ self externalProjects 
		detect: [ :each | each name = aName ]
]

{ #category : 'accessing' }
BaselineOfPharo class >> externalProjects [
	<script: 'self externalProjects inspect'>

	^ ExternalProjects ifNil: [ 
		ExternalProjects := self declareExternalProjects ]
]

{ #category : 'accessing - external projects' }
BaselineOfPharo class >> iceberg [
	<externalProject>

	^ PharoExternalProject 
		newName: 'Iceberg' 
		owner: 'pharo-vcs' 
		project: 'iceberg' 
		version: 'Pharo13' 
		sourceDir: nil
]

{ #category : 'repository urls' }
BaselineOfPharo class >> icebergRepository [

	^ (self externalProjectNamed: 'Iceberg') repository
]

{ #category : 'accessing - external projects' }
BaselineOfPharo class >> microdown [
	<externalProject>
	
	^ PharoExternalProject 
		newName: 'Microdown' 
		owner: 'pillar-markup' 
		project: 'Microdown' 
		version: 'Pharo13'
]

{ #category : 'repository urls' }
BaselineOfPharo class >> microdownRepository [

	^ (self externalProjectNamed: 'Microdown') repository
]

{ #category : 'accessing - external projects' }
BaselineOfPharo class >> newTools [
	<externalProject>

	^ PharoExternalProject 
		newName: 'NewTools' 
		owner: 'pharo-spec' 
		project: 'NewTools' 
		version: 'Pharo13'
]

{ #category : 'repository urls' }
BaselineOfPharo class >> newToolsRepository [

	^ (self externalProjectNamed: 'NewTools') repository
]

{ #category : 'class initialization' }
BaselineOfPharo class >> reset [
	<script>

	ExternalProjects := nil
]

{ #category : 'accessing - external projects' }
BaselineOfPharo class >> roassal [
	<externalProject>

	^ PharoExternalProject
		  newName: 'Roassal'
		  owner: 'pharo-graphics'
		  project: 'Roassal'
		  version: 'v1.06f'
]

{ #category : 'repository urls' }
BaselineOfPharo class >> roassalRepository [

	^ (self externalProjectNamed: 'Roassal') repository
]

{ #category : 'accessing - external projects' }
BaselineOfPharo class >> scriptableDebugger [
	<externalProject>

	^ PharoExternalProject 
		newName: 'ScriptableDebugger' 
		owner: 'pharo-spec' 
		project: 'ScriptableDebugger' 
		version: 'Pharo13'
]

{ #category : 'repository urls' }
BaselineOfPharo class >> scriptableDebuggerRepository [

	^ (self externalProjectNamed: 'ScriptableDebugger') repository
]

{ #category : 'accessing - external projects' }
BaselineOfPharo class >> spec [
	<externalProject>

	^ PharoExternalProject 
		newName: 'Spec2' 
		owner: 'pharo-spec' 
		project:'Spec' 
		version: 'Pharo13'
]

{ #category : 'repository urls' }
BaselineOfPharo class >> specRepository [

	^ (self externalProjectNamed: 'Spec2') repository
]

{ #category : 'accessing - external projects' }
BaselineOfPharo class >> toplo [
	<externalProject>
	
	^ PharoExternalProject 
		newName: 'Toplo' 
		owner: 'pharo-graphics' 
		project: 'Toplo' 
		version: 'Pharo12'
]

{ #category : 'repository urls' }
BaselineOfPharo class >> toploRepository [

	^ (self externalProjectNamed: 'Toplo') repository
]

{ #category : 'accessing - external projects' }
BaselineOfPharo class >> welcomeBrowser [
	<externalProject>
	
	^ PharoExternalProject 
		newName: 'WelcomeBrowser' 
		owner: 'pharo-spec' 
		project: 'NewTools-WelcomeBrowser' 
		version: 'Pharo13'
]

{ #category : 'repository urls' }
BaselineOfPharo class >> welcomeBrowserRepository [

	^ (self externalProjectNamed: 'WelcomeBrowser') repository
]

{ #category : 'baselines' }
BaselineOfPharo >> baseline: spec [

	<baseline>
	| repository |
	repository := self packageRepositoryURLForSpec: spec.

	spec for: #common do: [
		spec postLoadDoIt: #postload:package:.
		spec baseline: 'BaseLibraries' with: [ spec repository: repository ].

		spec baseline: 'IDE' with: [ spec repository: repository ].
		spec baseline: 'Calypso' with: [
			spec
				repository: repository;
				loads: #( 'IcebergSupport' ) ].

		spec package: 'Deprecated13' ]
]

{ #category : 'actions' }
BaselineOfPharo >> postload: loader package: packageSpec [
	"If we added a github token for the build, we remove it."

	Smalltalk os environment at: #GITHUB_TOKEN ifPresent: [ :token |
		| credential |
		credential := (Smalltalk classNamed: #IceCredentialStore) current plaintextCredentialForHostname: 'github.com'.
		credential password = token ifTrue: [ (Smalltalk classNamed: #IceCredentialStore) current removePlainTextCredential: credential ].
		'Removing credential.' traceCr ].

	"Open the WelcomeBrowser as last step"
	(self class environment classNamed: #StWelcomeBrowser)
		ifNotNil: [ :aClass | aClass openForRelease ]
]

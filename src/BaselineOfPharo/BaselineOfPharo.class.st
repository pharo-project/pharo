Class {
	#name : #BaselineOfPharo,
	#superclass : #BaselineOf,
	#classVars : [
		'ExternalProjects'
	],
	#category : #BaselineOfPharo
}

{ #category : #'repository urls' }
BaselineOfPharo class >> beautifulCommentsRepository [

	^ (self externalProjectNamed: 'BeautifulComments') repository
]

{ #category : #'external projects' }
BaselineOfPharo class >> declareExternalProjects [

	^ { 
	PharoExternalProject newName: 'Iceberg' owner: 'pharo-vcs' project: 'iceberg' version: 'dev-2.0' sourceDir: nil.
	PharoExternalProject newName: 'Spec2' owner: 'pharo-spec' project:'Spec' version: 'Pharo10'.
	PharoExternalProject newName: 'NewTools' owner: 'pharo-spec' project: 'NewTools' version: 'Pharo10'.	
	PharoExternalProject newName: 'Microdown' owner: 'pillar-markup' project: 'Microdown' version: 'integration'.	
	PharoExternalProject newName: 'BeautifulComments' owner: 'pillar-markup' project: 'BeautifulComments' version: 'integration'.	
	PharoExternalProject newName: 'Roassal3' owner: 'ObjectProfile' project: 'Roassal3' version: 'v0.9.9f'.	
	 }
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

{ #category : #'repository urls' }
BaselineOfPharo class >> icebergRepository [

	^ (self externalProjectNamed: 'Iceberg') repository
]

{ #category : #versions }
BaselineOfPharo class >> icebergVersion [

	^ 'dev-2.0'
]

{ #category : #'repository urls' }
BaselineOfPharo class >> microdownRepository [

	^ (self externalProjectNamed: 'Microdown') repository
]

{ #category : #'repository urls' }
BaselineOfPharo class >> newToolsRepository [

	^ (self externalProjectNamed: 'NewTools') repository
]

{ #category : #versions }
BaselineOfPharo class >> newToolsVersion [

	^ 'Pharo10'
]

{ #category : #'repository urls' }
BaselineOfPharo class >> roassal3Repository [

	^ (self externalProjectNamed: 'Roassal3') repository
]

{ #category : #'repository urls' }
BaselineOfPharo class >> specRepository [

	^ (self externalProjectNamed: 'Spec2') repository
]

{ #category : #versions }
BaselineOfPharo class >> specVersion [

	^ 'Pharo10'
]

{ #category : #baseline }
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

		spec package: 'Deprecated10'.
		
		spec package: 'FluidClassBuilder'.
		spec package: 'FluidClassBuilder-Tests' with: [ spec requires: 'FluidClassBuilder' ].
	]
]

{ #category : #baseline }
BaselineOfPharo >> postload: loader package: packageSpec [

	WelcomeHelp openForRelease
]

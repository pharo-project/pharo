Class {
	#name : #BaselineOfPharo,
	#superclass : #BaselineOf,
	#category : #BaselineOfPharo
}

{ #category : #'repository urls' }
BaselineOfPharo class >> icebergRepository [

	^ 'github://pharo-vcs/iceberg:{1}' format: { self icebergVersion }
]

{ #category : #versions }
BaselineOfPharo class >> icebergVersion [

	^ 'dev-2.0'
]

{ #category : #versions }
BaselineOfPharo class >> newToolsReleaseCommit [
	"The commit corresponding to current release (to be used on bootstrap process)"

	"Pharo10"
	^ '0a30d62222168b54ba9c12608bd10a7c1cfed6a1'
]

{ #category : #'repository urls' }
BaselineOfPharo class >> newToolsRepository [

	^ 'github://pharo-spec/NewTools:{1}/src' format: { self newToolsVersion }
]

{ #category : #versions }
BaselineOfPharo class >> newToolsVersion [

	^ 'Pharo10'
]

{ #category : #versions }
BaselineOfPharo class >> specReleaseCommit [
	"The commit corresponding to current release (to be used on bootstrap process)"

	"Pharo10- fix paginator in presence of fast dragging"
	^ '8fe65f1d49f090bb739ae6ef5113a05b4e89109f'
]

{ #category : #'repository urls' }
BaselineOfPharo class >> specRepository [

	^ 'github://pharo-spec/Spec:{1}/src' format: { self specVersion }
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

		spec package: 'Deprecated90'.
		spec package: 'Deprecated10'.
		
		spec package: 'FluidClassBuilder'.
		spec package: 'FluidClassBuilder-Tests' with: [ spec requires: 'FluidClassBuilder' ].
	]
]

{ #category : #baseline }
BaselineOfPharo >> postload: loader package: packageSpec [

	WelcomeHelp openForRelease
]

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

	^ 'v2.0.5'
]

{ #category : #versions }
BaselineOfPharo class >> newToolsReleaseCommit [
	"The commit corresponding to current release (to be used on bootstrap process)"
	
	"v0.6.10"
	^ '53f0f8ba1c81edd16b994bb80f8568e2693d5faf'
]

{ #category : #'repository urls' }
BaselineOfPharo class >> newToolsRepository [

	^ 'github://pharo-spec/NewTools:{1}/src' format: { self newToolsVersion }
]

{ #category : #versions }
BaselineOfPharo class >> newToolsVersion [

	^ 'v0.6.10'
]

{ #category : #versions }
BaselineOfPharo class >> specReleaseCommit [
	"The commit corresponding to current release (to be used on bootstrap process)"

	"v1.0.3"
	^ '64d914cd5c621f00b551306ee2fbd1e3540af6a1'
]

{ #category : #'repository urls' }
BaselineOfPharo class >> specRepository [

	^ 'github://pharo-spec/Spec:{1}/src' format: { self specVersion }
]

{ #category : #versions }
BaselineOfPharo class >> specVersion [

	^ 'v1.0.3'
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
		
		spec package: 'FluidClassBuilder'.
		spec package: 'FluidClassBuilder-Tests' with: [ spec requires: 'FluidClassBuilder' ].
	]
]

{ #category : #baseline }
BaselineOfPharo >> postload: loader package: packageSpec [

	WelcomeHelp openForRelease
]

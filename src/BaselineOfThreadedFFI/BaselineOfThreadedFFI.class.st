Class {
	#name : 'BaselineOfThreadedFFI',
	#superclass : 'BaselineOf',
	#category : 'BaselineOfThreadedFFI',
	#package : 'BaselineOfThreadedFFI'
}

{ #category : 'accessing' }
BaselineOfThreadedFFI class >> corePackageNames [

	^ self packagesOfGroupNamed: #Core
]

{ #category : 'baseline' }
BaselineOfThreadedFFI >> baseline: spec [

	<baseline>
	| repository |
	repository := self packageRepositoryURLForSpec: spec.
	spec for: #common do: [
			spec postLoadDoIt: #reset.

			"ThreadedFFI extends Bitmap with tfPointerAddress"
			spec baseline: 'Display' with: [ spec loads: #( 'Core' ) from: repository ].

			spec
				package: 'ThreadedFFI' with: [ spec requires: #( 'Display' ) ];
				package: 'ThreadedFFI-UFFI' with: [ spec requires: #( 'ThreadedFFI' ) ];
				package: 'ThreadedFFI-Tests' with: [ spec requires: #( 'ThreadedFFI' ) ];
				package: 'ThreadedFFI-UFFI-Tests' with: [ "core" spec requires: #( 'ThreadedFFI-UFFI' ) ] ].

	spec
		group: 'Core' with: #( 'ThreadedFFI' 'ThreadedFFI-UFFI' );
		group: 'Tests' with: #( 'ThreadedFFI-Tests' 'ThreadedFFI-UFFI-Tests' )
]

{ #category : 'do-its' }
BaselineOfThreadedFFI >> reset [

	self classNamed: #FFIMethodRegistry ifPresent: [ :class | class resetAll ].

	self classNamed: #FFIBackend ifPresent: [ :class | class reset ]
]

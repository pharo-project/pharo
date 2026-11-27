Class {
	#name : 'BaselineOfUnifiedFFI',
	#superclass : 'BaselineOf',
	#category : 'BaselineOfUnifiedFFI',
	#package : 'BaselineOfUnifiedFFI'
}

{ #category : 'baselines' }
BaselineOfUnifiedFFI >> baseline: spec [

	<baseline>
	| repository |
	repository := self packageRepositoryURLForSpec: spec.
	spec for: #common do: [
			spec postLoadDoIt: #reset.

			"UnifiedFFI extends Bitmap with tfPointerAddress"
			spec baseline: 'Display' with: [ spec loads: #( 'Core' ) from: repository ].

			"The tests decompile FFI methods"
			spec baseline: 'Flashback' with: [ spec loads: #( 'Core' ) from: repository ].

			spec
				package: 'UnifiedFFI' with: [ spec requires: #( 'Display' ) ];
				package: 'UnifiedFFI-Tests' with: [ spec requires: #( 'UnifiedFFI' 'System-OSEnvironments' 'Flashback' ) ];
				package: 'System-OSEnvironments' with: [ spec requires: #( 'UnifiedFFI' ) ];
				package: 'System-OSEnvironments-Tests' with: [ spec requires: #( 'System-OSEnvironments' ) ].

			spec
				group: 'default' with: #( 'core' );
				group: 'minimal' with: #( 'UnifiedFFI' 'System-OSEnvironments' );
				group: 'core' with: #( 'minimal' 'Tests' );
				group: 'Tests' with: #( 'UnifiedFFI-Tests' 'System-OSEnvironments-Tests' ) ]
]

{ #category : 'do-its' }
BaselineOfUnifiedFFI >> reset [

	self classNamed: #FFIMethodRegistry ifPresent: [ :class | class resetAll ].

	self classNamed: #FFIBackend ifPresent: [ :class | class reset ]
]

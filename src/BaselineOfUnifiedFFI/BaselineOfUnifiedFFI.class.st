Class {
	#name : 'BaselineOfUnifiedFFI',
	#superclass : 'BaselineOf',
	#category : 'BaselineOfUnifiedFFI',
	#package : 'BaselineOfUnifiedFFI'
}

{ #category : 'baseline' }
BaselineOfUnifiedFFI >> baseline: spec [

	<baseline>
	spec for: #common do: [
			spec
				package: 'UnifiedFFI';
				package: 'UnifiedFFI-Tests' with: [ spec requires: #( 'UnifiedFFI' ) ];
				package: 'System-OSEnvironments' with: [ spec requires: #( 'UnifiedFFI' ) ];
				package: 'Multilingual-Encodings' with: [ spec requires: #( 'System-OSEnvironments' ) ]. "This package should probably not be here, but it depends on System-OSEnvironments "

			spec
				group: 'default' with: #( 'core' );
				group: 'minimal' with: #( 'UnifiedFFI' 'System-OSEnvironments' 'Multilingual-Encodings');
				group: 'core' with: #( 'minimal' 'UnifiedFFI-Tests' ) ]
]

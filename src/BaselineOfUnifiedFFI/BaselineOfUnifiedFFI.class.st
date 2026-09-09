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
			"unified ffi"
			package: 'UnifiedFFI';
			package: 'UnifiedFFI-Tests' with: [ spec requires: #('UnifiedFFI') ];
			package: 'System-OSEnvironments' with: [ spec requires: #('UnifiedFFI') ];
			package: 'System-OSEnvironments-Tests' with: [ spec requires: #('System-OSEnvironments') ].
			
		spec 
			group: 'default' with: #('core');
			group: 'minimal' with: #('UnifiedFFI' 'System-OSEnvironments');
			group: 'core' with: #('minimal' 'UnifiedFFI-Tests' 'System-OSEnvironments-Tests') ]
]

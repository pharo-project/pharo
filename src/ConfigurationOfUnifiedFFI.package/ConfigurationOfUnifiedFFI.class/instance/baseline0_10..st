baselines
baseline0_10: spec
	<version: '0.10-baseline'>
	
	spec for: #common do: [
		spec blessing: #baseline.
		spec repository: 'http://www.smalltalkhub.com/mc/Pharo/FFI-NB/main'. 

		self ffi: spec.
		self alien: spec.
		
		spec 
			package: 'UnifiedFFI' with: [ spec requires: #('FFI' 'Alien') ];
			package: 'UnifiedFFI-Tests' with: [ spec requires: #('UnifiedFFI') ];
			package: 'UnifiedFFI-Legacy' with: [ spec requires: #('UnifiedFFI') ].
			
		spec 
			group: 'default' with: #('core' 'legacy');
			group: 'core' with: #('UnifiedFFI' 'UnifiedFFI-Tests');
			group: 'legacy' with: #('UnifiedFFI-Legacy') ]
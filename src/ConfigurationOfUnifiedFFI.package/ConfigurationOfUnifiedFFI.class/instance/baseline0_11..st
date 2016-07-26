baselines
baseline0_11: spec
	<version: '0.11-baseline'>
	
	spec for: #common do: [
		spec blessing: #baseline.
		spec repository: 'http://www.smalltalkhub.com/mc/Pharo/FFI-NB/main'. 

		spec 
			"ffi"
			package: 'FFI-Pools' with: [ spec repository: 'http://source.squeakfoundation.org/FFI' ];
			package: 'FFI-Kernel' with: [ 
				spec 
					requires: #('FFI-Pools');
					repository: 'http://source.squeakfoundation.org/FFI' ];
			package: 'FFI-Tests' with: [ 
				spec 
					requires: #('FFI-Kernel');
					repository: 'http://source.squeakfoundation.org/FFI' ];
			"alien"
			package: 'Alien' with: [ 
				spec 
					requires: #('FFI-Kernel');
					repository: 'http://www.squeaksource.com/Alien' ];
			"unified ffi"
			package: 'UnifiedFFI' with: [ spec requires: #('FFI-Kernel' 'Alien') ];
			package: 'UnifiedFFI-Tests' with: [ spec requires: #('UnifiedFFI') ];
			package: 'UnifiedFFI-Legacy' with: [ spec requires: #('UnifiedFFI') ].
			
		spec 
			group: 'default' with: #('core' 'legacy');
			group: 'core' with: #('UnifiedFFI' 'UnifiedFFI-Tests');
			group: 'legacy' with: #('UnifiedFFI-Legacy') ]
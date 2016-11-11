baseline
baseline: spec
	<baseline>
	
	spec for: #common do: [
		spec preLoadDoIt: #'preload:package:'.
		spec postLoadDoIt: #'postload:package:'.
		spec 
			"ffi"
			package: 'FFI-Pools';
			package: 'FFI-Kernel' with: [ spec requires: #('FFI-Pools') ];
			"alien"
			package: 'Alien' with: [ spec requires: #('FFI-Kernel') ];
			"unified ffi"
			package: 'UnifiedFFI' with: [ spec requires: #('FFI-Kernel' 'Alien') ];
			package: 'UnifiedFFI-Tests' with: [ spec requires: #('UnifiedFFI') ];
			package: 'UnifiedFFI-Legacy' with: [ spec requires: #('UnifiedFFI') ].
			
		spec 
			group: 'default' with: #('core' 'legacy');
			group: 'core' with: #('UnifiedFFI' 'UnifiedFFI-Tests');
			group: 'legacy' with: #('UnifiedFFI-Legacy') ]
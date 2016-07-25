versions
v0_20_2: spec
	<version: '0.20.2' imports: #('0.13-baseline' )>

	spec for: #'common' do: [
		spec blessing: #'stable'.
		spec description: 'fix deprecated ExternalObject initialization
fix external allocation of structures
fix alignment for strings'.
		spec author: 'EstebanLorenzano'.
		spec timestamp: '5/30/2016 17:00'.
		spec 
			package: 'FFI-Pools' with: 'FFI-Pools-eem.3';
			package: 'FFI-Kernel' with: 'FFI-Kernel-EstebanLorenzano.41';
			package: 'Alien' with: 'Alien-eem.32';
			package: 'UnifiedFFI' with: 'UnifiedFFI-EstebanLorenzano.60';
			package: 'UnifiedFFI-Tests' with: 'UnifiedFFI-Tests-TheIntegrator.26';
			package: 'UnifiedFFI-Legacy' with: 'UnifiedFFI-Legacy-EstebanLorenzano.3'. ].

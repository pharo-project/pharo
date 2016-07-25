versions
v0_13: spec
	<version: '0.13' imports: #('0.12-baseline' )>

	spec for: #'common' do: [
		spec blessing: #'stable'.
		spec description: 'atomic types can now be rolled into pointers too (readonly)'.
		spec author: 'EstebanLorenzano'.
		spec timestamp: '2016-02-10 10:00'.
		spec 
			package: 'FFI-Pools' with: 'FFI-Pools-eem.3';
			package: 'FFI-Kernel' with: 'FFI-Kernel-EstebanLorenzano.29';
			package: 'Alien' with: 'Alien-eem.28';
			package: 'UnifiedFFI' with: 'UnifiedFFI-EstebanLorenzano.17';
			package: 'UnifiedFFI-Tests' with: 'UnifiedFFI-Tests-EstebanLorenzano.6';
			package: 'UnifiedFFI-Legacy' with: 'UnifiedFFI-Legacy-EstebanLorenzano.1' ]
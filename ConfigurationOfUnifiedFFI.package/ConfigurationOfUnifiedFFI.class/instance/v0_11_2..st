versions
v0_11_2: spec
	<version: '0.11.2' imports: #('0.11-baseline' )>

	spec for: #'common' do: [
		spec blessing: #'stable'.
		spec description: 'Update Alien'.
		spec author: 'EstebanLorenzano'.
		spec timestamp: '2016-01-27 15:15'.
		spec 
			package: 'FFI-Pools' with: 'FFI-Pools-eem.3';
			package: 'FFI-Kernel' with: 'FFI-Kernel-EstebanLorenzano.29';
			package: 'FFI-Tests' with: 'FFI-Tests-EstebanLorenzano.8';
			package: 'Alien' with: 'Alien-eem.28';
			package: 'UnifiedFFI' with: 'UnifiedFFI-EstebanLorenzano.10';
			package: 'UnifiedFFI-Tests' with: 'UnifiedFFI-Tests-EstebanLorenzano.3';
			package: 'UnifiedFFI-Legacy' with: 'UnifiedFFI-Legacy-EstebanLorenzano.1' ]
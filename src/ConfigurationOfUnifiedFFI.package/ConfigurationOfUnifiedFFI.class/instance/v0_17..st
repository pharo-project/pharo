versions
v0_17: spec
	<version: '0.17' imports: #('0.13-baseline' )>

	spec for: #'common' do: [
		spec blessing: #'stable'.
		spec description: 'added support for HANDLE types (FFIConstantHandle)'.
		spec author: 'EstebanLorenzano'.
		spec timestamp: '2016-03-22 21:20'.
		spec 
			package: 'FFI-Pools' with: 'FFI-Pools-eem.3';
			package: 'FFI-Kernel' with: 'FFI-Kernel-EstebanLorenzano.39';
			package: 'Alien' with: 'Alien-eem.32';
			package: 'UnifiedFFI' with: 'UnifiedFFI-EstebanLorenzano.47';
			package: 'UnifiedFFI-Legacy' with: 'UnifiedFFI-Legacy-EstebanLorenzano.3';
			package: 'UnifiedFFI-Tests' with: 'UnifiedFFI-Tests-EstebanLorenzano.18' ]
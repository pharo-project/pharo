versions
v0_13_1: spec
	<version: '0.13.1' imports: #('0.12-baseline' )>

	spec for: #'common' do: [
		spec blessing: #'stable'.
		spec description: '-float types can be rolled as pointers correctly
- updated FFI-Kernel'.
		spec author: 'EstebanLorenzano'.
		spec timestamp: '2016-02-19 15:10'.
		spec 
			package: 'FFI-Pools' with: 'FFI-Pools-eem.3';
			package: 'FFI-Kernel' with: 'FFI-Kernel-EstebanLorenzano.32';
			package: 'Alien' with: 'Alien-eem.28';
			package: 'UnifiedFFI' with: 'UnifiedFFI-EstebanLorenzano.20';
			package: 'UnifiedFFI-Tests' with: 'UnifiedFFI-Tests-EstebanLorenzano.6';
			package: 'UnifiedFFI-Legacy' with: 'UnifiedFFI-Legacy-EstebanLorenzano.1' ]
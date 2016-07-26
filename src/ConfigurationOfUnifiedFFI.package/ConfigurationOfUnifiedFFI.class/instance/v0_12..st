versions
v0_12: spec
	<version: '0.12' imports: #('0.12-baseline' )>

	spec for: #'common' do: [
		spec blessing: #'stable'.
		spec description: '- prepared to work with internalised FFI (just fix tests :P)
- remove FFI-Tests because is not what we want.'.
		spec author: 'EstebanLorenzano'.
		spec timestamp: '2016-02-05 17:15'.
		spec 
			package: 'FFI-Pools' with: 'FFI-Pools-eem.3';
			package: 'FFI-Kernel' with: 'FFI-Kernel-EstebanLorenzano.29';
			package: 'Alien' with: 'Alien-eem.28';
			package: 'UnifiedFFI' with: 'UnifiedFFI-EstebanLorenzano.15';
			package: 'UnifiedFFI-Tests' with: 'UnifiedFFI-Tests-EstebanLorenzano.6';
			package: 'UnifiedFFI-Legacy' with: 'UnifiedFFI-Legacy-EstebanLorenzano.1' ]
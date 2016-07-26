versions
v0_12_1: spec
	<version: '0.12.1' imports: #('0.12-baseline' )>

	spec for: #'common' do: [
		spec blessing: #'stable'.
		spec description: '- fix unroll when external function answer NULL
- FFIOop types can be used in callbacks and arrays'.
		spec author: 'EstebanLorenzano'.
		spec timestamp: '2016-02-08 14:30'.
		spec 
			package: 'FFI-Pools' with: 'FFI-Pools-eem.3';
			package: 'FFI-Kernel' with: 'FFI-Kernel-EstebanLorenzano.29';
			package: 'Alien' with: 'Alien-eem.28';
			package: 'UnifiedFFI' with: 'UnifiedFFI-EstebanLorenzano.16';
			package: 'UnifiedFFI-Tests' with: 'UnifiedFFI-Tests-EstebanLorenzano.6';
			package: 'UnifiedFFI-Legacy' with: 'UnifiedFFI-Legacy-EstebanLorenzano.1' ]
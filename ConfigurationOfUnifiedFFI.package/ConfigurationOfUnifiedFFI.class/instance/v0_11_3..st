versions
v0_11_3: spec
	<version: '0.11.3' imports: #('0.11-baseline' )>

	spec for: #'common' do: [
		spec blessing: #'stable'.
		spec description: '- added ExternalAddress and ByteArray as oop (void *) classes.
- bugfixes'.
		spec author: 'EstebanLorenzano'.
		spec timestamp: '2016-02-04 14:15'.
		spec 
			package: 'FFI-Pools' with: 'FFI-Pools-eem.3';
			package: 'FFI-Kernel' with: 'FFI-Kernel-EstebanLorenzano.29';
			package: 'FFI-Tests' with: 'FFI-Tests-EstebanLorenzano.8';
			package: 'Alien' with: 'Alien-eem.28';
			package: 'UnifiedFFI' with: 'UnifiedFFI-EstebanLorenzano.15';
			package: 'UnifiedFFI-Tests' with: 'UnifiedFFI-Tests-EstebanLorenzano.4';
			package: 'UnifiedFFI-Legacy' with: 'UnifiedFFI-Legacy-EstebanLorenzano.1' ]
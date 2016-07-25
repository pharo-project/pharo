versions
v0_20: spec
	<version: '0.20' imports: #('0.13-baseline' )>

	spec for: #'common' do: [
		spec blessing: #'stable'.
		spec description: '- Added support for type alignment.
- Fix the computation of the layout FFIExternalStructure by properly aligning the fields.
- Added FFIExternalPackedStructure and FFIExternalUnion.'.
		spec author: 'RonieSalgado'.
		spec timestamp: '5/4/2016 14:54'.
		spec 
			package: 'FFI-Pools' with: 'FFI-Pools-eem.3';
			package: 'FFI-Kernel' with: 'FFI-Kernel-JanVanDeSandt.40';
			package: 'Alien' with: 'Alien-eem.32';
			package: 'UnifiedFFI' with: 'UnifiedFFI-RonieSalgado.58';
			package: 'UnifiedFFI-Tests' with: 'UnifiedFFI-Tests-RonieSalgado.25';
			package: 'UnifiedFFI-Legacy' with: 'UnifiedFFI-Legacy-EstebanLorenzano.3'. ].

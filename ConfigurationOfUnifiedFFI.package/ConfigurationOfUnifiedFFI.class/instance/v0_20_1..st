versions
v0_20_1: spec
	<version: '0.20.1' imports: #('0.13-baseline' )>

	spec for: #'common' do: [
		spec blessing: #'stable'.
		spec description: '- fix deprecated ExternalObject initialization'.
		spec author: 'PavelKrivanek'.
		spec timestamp: '5/9/2016 16:30'.
		spec 
			package: 'FFI-Pools' with: 'FFI-Pools-eem.3';
			package: 'FFI-Kernel' with: 'FFI-Kernel-PavelKrivanek.41';
			package: 'Alien' with: 'Alien-eem.32';
			package: 'UnifiedFFI' with: 'UnifiedFFI-RonieSalgado.58';
			package: 'UnifiedFFI-Tests' with: 'UnifiedFFI-Tests-RonieSalgado.25';
			package: 'UnifiedFFI-Legacy' with: 'UnifiedFFI-Legacy-EstebanLorenzano.3'. ].

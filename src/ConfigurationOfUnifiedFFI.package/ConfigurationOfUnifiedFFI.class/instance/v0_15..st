versions
v0_15: spec
	<version: '0.15' imports: #('0.13-baseline' )>

	spec for: #'common' do: [
		spec blessing: #'stable'.
		spec description: '- FFIExternalValueHolder now can be shadowed (ad old NB)
- added support for booleanAt: and booleanAt:put:
- fix a couple of bugs'.
		spec author: 'EstebanLorenzano'.
		spec timestamp: '2016-03-10 22:30'.
		spec 
			package: 'FFI-Pools' with: 'FFI-Pools-eem.3';
			package: 'FFI-Kernel' with: 'FFI-Kernel-EstebanLorenzano.39';
			package: 'Alien' with: 'Alien-EstebanLorenzano.31';
			package: 'UnifiedFFI' with: 'UnifiedFFI-EstebanLorenzano.35';
			package: 'UnifiedFFI-Legacy' with: 'UnifiedFFI-Legacy-EstebanLorenzano.2';
			package: 'UnifiedFFI-Tests' with: 'UnifiedFFI-Tests-EstebanLorenzano.12' ]
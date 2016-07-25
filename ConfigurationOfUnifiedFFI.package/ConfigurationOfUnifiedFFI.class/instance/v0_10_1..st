versions
v0_10_1: spec
	<version: '0.10.1' imports: #('0.10-baseline' )>

	spec for: #'common' do: [
		spec blessing: #'stable'.
		spec description: 'fixed #sizeOf: method'.
		spec author: 'EstebanLorenzano'.
		spec timestamp: '2016-01-20 16:10'.
		spec 
			project: 'FFI' with: '1.10.1';
			project: 'Alien' with: '1.2'.
		spec 
			package: 'UnifiedFFI' with: 'UnifiedFFI-EstebanLorenzano.2';
			package: 'UnifiedFFI-Tests' with: 'UnifiedFFI-Tests-EstebanLorenzano.1';
			package: 'UnifiedFFI-Legacy' with: 'UnifiedFFI-Legacy-EstebanLorenzano.1' ].
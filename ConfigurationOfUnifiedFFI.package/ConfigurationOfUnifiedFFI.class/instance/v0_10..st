versions
v0_10: spec
	<version: '0.10' imports: #('0.10-baseline' )>

	spec for: #'common' do: [
		spec blessing: #'stable'.
		spec description: 'renamed as UnifiedFFI'.
		spec author: 'EstebanLorenzano'.
		spec timestamp: '2016-01-18 22:39'.
		spec 
			project: 'FFI' with: '1.10.1';
			project: 'Alien' with: '1.2'.
		spec 
			package: 'UnifiedFFI' with: 'UnifiedFFI-EstebanLorenzano.1';
			package: 'UnifiedFFI-Tests' with: 'UnifiedFFI-Tests-EstebanLorenzano.1';
			package: 'UnifiedFFI-Legacy' with: 'UnifiedFFI-Legacy-EstebanLorenzano.1' ].
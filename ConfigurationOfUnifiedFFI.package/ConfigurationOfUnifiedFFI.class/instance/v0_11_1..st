versions
v0_11_1: spec
	<version: '0.11.1' imports: #('0.10-baseline' )>

	spec for: #'common' do: [
		spec blessing: #'stable'.
		spec description: 'Fixed tests for platforms other than macs :)'.
		spec author: 'EstebanLorenzano'.
		spec timestamp: '2016-01-25 13:00'.
		spec 
			project: 'FFI' with: '1.10.1';
			project: 'Alien' with: '1.2'.
		spec 
			package: 'UnifiedFFI' with: 'UnifiedFFI-EstebanLorenzano.10';
			package: 'UnifiedFFI-Tests' with: 'UnifiedFFI-Tests-EstebanLorenzano.3';
			package: 'UnifiedFFI-Legacy' with: 'UnifiedFFI-Legacy-EstebanLorenzano.1' ]
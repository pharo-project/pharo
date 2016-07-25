versions
v0_11: spec
	<version: '0.11' imports: #('0.10-baseline' )>

	spec for: #'common' do: [
		spec blessing: #'stable'.
		spec description: 'added interpretation of pointer arities bigger than 1'.
		spec author: 'EstebanLorenzano'.
		spec timestamp: '2016-01-24 12:30'.
		spec 
			project: 'FFI' with: '1.10.1';
			project: 'Alien' with: '1.2'.
		spec 
			package: 'UnifiedFFI' with: 'UnifiedFFI-EstebanLorenzano.10';
			package: 'UnifiedFFI-Tests' with: 'UnifiedFFI-Tests-EstebanLorenzano.2';
			package: 'UnifiedFFI-Legacy' with: 'UnifiedFFI-Legacy-EstebanLorenzano.1' ]
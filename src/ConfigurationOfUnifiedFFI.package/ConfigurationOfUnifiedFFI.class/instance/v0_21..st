versions
v0_21: spec
	<version: '0.21' imports: #('0.13-baseline' )>

	spec for: #'common' do: [
		spec blessing: #'stable'.
		spec description: 'FFICallback updated to work also with other platforms (not tested, but prepared :)'.
		spec author: 'EstebanLorenzano'.
		spec timestamp: '2016-06-28 16:30'.
		spec 
			package: 'FFI-Pools' with: 'FFI-Pools-eem.3';
			package: 'FFI-Kernel' with: 'FFI-Kernel-EstebanLorenzano.41';
			package: 'Alien' with: 'Alien-eem.35';
			package: 'UnifiedFFI' with: 'UnifiedFFI-EstebanLorenzano.64';
			package: 'UnifiedFFI-Tests' with: 'UnifiedFFI-Tests-MerwanOuddane.27';
			package: 'UnifiedFFI-Legacy' with: 'UnifiedFFI-Legacy-EstebanLorenzano.3' ]
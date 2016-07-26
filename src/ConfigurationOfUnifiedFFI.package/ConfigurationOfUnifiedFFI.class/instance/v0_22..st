versions
v0_22: spec
	<version: '0.22' imports: #('0.13-baseline' )>

	spec for: #'common' do: [
		spec blessing: #'stable'.
		spec description: 'external references can be accessed from arrays too'.
		spec author: 'EstebanLorenzano'.
		spec timestamp: '2016-07-01 14:30'.
		spec 
			package: 'FFI-Pools' with: 'FFI-Pools-eem.3';
			package: 'FFI-Kernel' with: 'FFI-Kernel-EstebanLorenzano.41';
			package: 'Alien' with: 'Alien-eem.35';
			package: 'UnifiedFFI' with: 'UnifiedFFI-EstebanLorenzano.66';
			package: 'UnifiedFFI-Tests' with: 'UnifiedFFI-Tests-MerwanOuddane.27';
			package: 'UnifiedFFI-Legacy' with: 'UnifiedFFI-Legacy-EstebanLorenzano.3' ]
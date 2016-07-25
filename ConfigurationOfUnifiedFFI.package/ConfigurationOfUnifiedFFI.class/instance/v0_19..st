versions
v0_19: spec
	<version: '0.19' imports: #('0.13-baseline' )>

	spec for: #'common' do: [
		spec blessing: #'stable'.
		spec description: '- added a strategy to handle different kind of function resolution (like indirects, callbacks, etc.) 
- added support for registering callbacks as functions (optCallbackCall)
- added first implementation (non efficient) for using arrays inside structures
'.
		spec author: 'EstebanLorenzano'.
		spec timestamp: '2016-04-22 15:50'.
		spec 
			package: 'FFI-Pools' with: 'FFI-Pools-eem.3';
			package: 'FFI-Kernel' with: 'FFI-Kernel-EstebanLorenzano.39';
			package: 'Alien' with: 'Alien-eem.32';
			package: 'UnifiedFFI' with: 'UnifiedFFI-EstebanLorenzano.54';
			package: 'UnifiedFFI-Legacy' with: 'UnifiedFFI-Legacy-EstebanLorenzano.3';
			package: 'UnifiedFFI-Tests' with: 'UnifiedFFI-Tests-EstebanLorenzano.22' ]
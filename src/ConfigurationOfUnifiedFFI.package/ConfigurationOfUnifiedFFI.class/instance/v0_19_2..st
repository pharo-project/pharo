versions
v0_19_2: spec
	<version: '0.19.2' imports: #('0.13-baseline' )>

	spec for: #'common' do: [
		spec blessing: #'stable'.
		spec description: '- fix #context ignored when creating a callout (FFICalloutAPI).
- fix ExternalAddress>>fromString: needs to add 0 at the end
- fix bad type conversion for unsignedByte'.
		spec author: 'EstebanLorenzano'.
		spec timestamp: '2016-04-27 17:00'.
		spec 
			package: 'FFI-Pools' with: 'FFI-Pools-eem.3';
			package: 'FFI-Kernel' with: 'FFI-Kernel-JanVanDeSandt.40';
			package: 'Alien' with: 'Alien-eem.32';
			package: 'UnifiedFFI' with: 'UnifiedFFI-EstebanLorenzano.57';
			package: 'UnifiedFFI-Legacy' with: 'UnifiedFFI-Legacy-EstebanLorenzano.3';
			package: 'UnifiedFFI-Tests' with: 'UnifiedFFI-Tests-EstebanLorenzano.24' ]
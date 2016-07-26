ConfigurationOfProjectToolBox
version108ProjectToolBox: spec
	<version: '1.0.8-baseline'>

	spec for: #'common' do: [
		spec 
			package: 'Example-Core';
			package: 'Example-Tests' with: [
				spec requires: #('Example-Core' ). ]. ].

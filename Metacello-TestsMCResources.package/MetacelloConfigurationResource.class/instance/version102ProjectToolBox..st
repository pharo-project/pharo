ConfigurationOfProjectToolBox
version102ProjectToolBox: spec
	<version: '1.0.2-baseline'>

	spec for: #'common' do: [
		spec preLoadDoIt: #'preloadForCore'.
		spec postLoadDoIt: #'postloadForCore:package:'.
		spec 
			package: 'Example-Core' with: [
				spec includes: #('Example-AddOn' ). ];
			package: 'Example-AddOn' with: [
				spec requires: #('Example-Core' ). ];
			package: 'Example-Tests' with: [
				spec requires: #('Example-AddOn' ). ]. ].

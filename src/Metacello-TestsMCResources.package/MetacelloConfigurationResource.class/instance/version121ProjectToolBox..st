ConfigurationOfProjectToolBox
version121ProjectToolBox: spec
	<version: '1.2.1-baseline'>

	spec for: #'common' do: [
		spec blessing: #'testBlessing'.
		spec repository: 'http://www.example.com/vor'.
		spec 
			package: 'Example-Core' with: [
				spec 
					includes: #('Example-AddOn' );
					file: 'Example-Core-anon.1';
					repository: 'http://www.example.com/or';
					preLoadDoIt: #'preloadForCore';
					postLoadDoIt: #'postloadForCore:package:'. ];
			package: 'Example-AddOn' with: [
				spec requires: #('Example-Core' ). ];
			package: 'Example-Tests' with: [
				spec requires: #('Example-AddOn' ). ]. ].

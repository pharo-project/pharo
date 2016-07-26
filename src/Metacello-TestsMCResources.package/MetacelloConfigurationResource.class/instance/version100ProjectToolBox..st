ConfigurationOfProjectToolBox
version100ProjectToolBox: spec
	<version: '1.0-baseline'>

	spec for: #'common' do: [
		spec blessing: #'testBlessing'.
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

ConfigurationOfProjectToolBox
version125ProjectToolBox: spec
	<version: '1.2.5-baseline' imports: #('1.0-baseline' )>

	spec for: #'common' do: [
		spec package: 'Example-Core' with: [
				spec 
					preLoadDoIt: #'preloadDoIt';
					postLoadDoIt: #'postloadDoIt'. ]. ].

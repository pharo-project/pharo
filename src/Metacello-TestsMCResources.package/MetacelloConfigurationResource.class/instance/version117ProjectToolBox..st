ConfigurationOfProjectToolBox
version117ProjectToolBox: spec
	<version: '1.1.7-baseline' imports: #('1.0-baseline' )>

	spec for: #'common' do: [
		spec package: 'Example-Core' overrides: [
				spec preLoadDoIt: #'alternatePreloadForCore'. ]. ].

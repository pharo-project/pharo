ConfigurationOfProjectToolBox
version119ProjectToolBox: spec
	<version: '1.1.9-baseline' imports: #('1.1.4-baseline' )>

	spec for: #'common' do: [
		spec project: 'Example Project' overrides: [
				spec
					className: 'MetacelloExampleProjectConfig';
					versionString: '1.0-baseline';
					preLoadDoIt: #'alternatePreloadForCore';
					repository: 'http://www.example.com/ob' ]. ].

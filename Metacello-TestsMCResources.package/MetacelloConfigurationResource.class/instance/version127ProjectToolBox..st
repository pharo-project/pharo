ConfigurationOfProjectToolBox
version127ProjectToolBox: spec
	<version: '1.2.7-baseline'>

	spec for: #'common' do: [
		spec project: 'Example Project' with: [
				spec
					className: 'MetacelloExampleProjectConfig';
					versionString: '1.0-baseline';
					preLoadDoIt: #'preloadDoIt';
					postLoadDoIt: #'postloadDoIt';
					repository: 'http://www.example.com/ob' ]. ].

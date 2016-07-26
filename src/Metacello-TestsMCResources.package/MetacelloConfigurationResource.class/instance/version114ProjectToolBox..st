ConfigurationOfProjectToolBox
version114ProjectToolBox: spec
	<version: '1.1.4-baseline'>

	spec for: #'common' do: [
		spec project: 'Example Project' with: [
				spec
					className: 'MetacelloExampleProjectConfig';
					versionString: '1.0-baseline';
					preLoadDoIt: #'preloadForProject';
					postLoadDoIt: #'postloadForProject';
					repository: 'http://www.example.com/ob' ]. ].

ConfigurationOfProjectToolBox
version129ProjectToolBox: spec
	<version: '1.2.9-baseline'>

	spec for: #'common' do: [
		spec 
			project: 'Example Project' with: [
				spec
					className: 'MetacelloExampleProjectConfig';
					versionString: '1.0-baseline';
					preLoadDoIt: #'preloadDoIt';
					postLoadDoIt: #'postloadDoIt';
					repository: 'http://www.example.com/ob' ];
			project: 'Copy Project' copyFrom: 'Example Project' with: [
				spec
					preLoadDoIt: nil;
					postLoadDoIt: nil ]. ].

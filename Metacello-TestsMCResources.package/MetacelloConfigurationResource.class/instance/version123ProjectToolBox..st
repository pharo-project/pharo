ConfigurationOfProjectToolBox
version123ProjectToolBox: spec
	<version: '1.2.3-baseline'>

	spec for: #'common' do: [
		spec project: 'Example Project' with: [
				spec
					className: 'MetacelloExampleProjectConfig';
					repository: 'http://www.example.com/ob';
					repository: 'http://www.example.com/or' ]. ].

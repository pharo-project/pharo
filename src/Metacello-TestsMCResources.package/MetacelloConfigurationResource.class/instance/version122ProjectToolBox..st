ConfigurationOfProjectToolBox
version122ProjectToolBox: spec
	<version: '1.2.2-baseline'>

	spec for: #'common' do: [
		spec project: 'Example Project' with: [
				spec
					className: 'MetacelloExampleProjectConfig';
					repository: 'http://www.example.com/ob' ]. ].

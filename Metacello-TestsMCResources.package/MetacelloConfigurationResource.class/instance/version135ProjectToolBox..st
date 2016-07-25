ConfigurationOfProjectToolBox
version135ProjectToolBox: spec
	<version: '1.3.5' imports: #('1.2.4-baseline' )>

	spec for: #'common' do: [
		spec repositories: [
			spec
				repository: 'http://www.example.com/ab';
				repository: 'http://www.example.com/ac' ]. ].

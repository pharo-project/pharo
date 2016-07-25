ConfigurationOfProjectToolBox
baselineVersion124ProjectToolBox: spec
	<version: '1.2.4-baseline'>

	spec for: #'common' do: [
		spec blessing: #'baseline'.
		spec repository: 'http://www.example.com/aa'. ].

	spec for: #'common' do: [
		spec repository: 'http://www.example.com/ab'. ].

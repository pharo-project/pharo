ConfigurationOfProjectToolBox
version111ProjectToolBox: spec
	<version: '1.1.1-baseline'>

	spec for: #'common' do: [
		spec package: 'Example-Core'.

		spec for: #'nested' do: [
			spec package: 'Example-Base'. ]. ].

	spec for: #'extra' do: [
		spec package: 'Example-Test'. ].

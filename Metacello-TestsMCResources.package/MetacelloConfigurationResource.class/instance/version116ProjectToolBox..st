ConfigurationOfProjectToolBox
version116ProjectToolBox: spec
	<version: '1.1.6-baseline' imports: #('1.1.4-baseline' )>

	spec for: #'common' do: [
		spec project: 'Example Project' with: [
				spec
					preLoadDoIt: #'alternatePreloadForCore' ]. ].

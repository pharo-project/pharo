ConfigurationOfProjectToolBox
version120ProjectToolBox: spec
	<version: '1.2.0-baseline' imports: #('1.1.4-baseline' )>

	spec for: #'common' do: [
		spec project: 'Copied Example' copyFrom: 'Example Project' with: [
				spec
					preLoadDoIt: #'alternatePreloadForCore' ]. ].

ConfigurationOfProjectToolBox
version128ProjectToolBox: spec
	<version: '1.2.8-baseline' imports: #('1.2.7-baseline' )>

	spec for: #'common' do: [
		spec project: 'Example Project' with: [
				spec
					preLoadDoIt: nil;
					postLoadDoIt: nil ]. ].

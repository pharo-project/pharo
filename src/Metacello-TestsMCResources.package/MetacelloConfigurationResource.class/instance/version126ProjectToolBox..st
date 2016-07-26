ConfigurationOfProjectToolBox
version126ProjectToolBox: spec
	<version: '1.2.6-baseline' imports: #('1.2.5-baseline' )>

	spec for: #'common' do: [
		spec package: 'Example-Core' with: [
				spec 
					preLoadDoIt: nil;
					postLoadDoIt: nil. ]. ].

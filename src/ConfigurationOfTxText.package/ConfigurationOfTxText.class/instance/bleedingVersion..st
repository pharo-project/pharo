versions
bleedingVersion: spec
	<version: 'bleeding' imports: #('0.6-baseline' )>

	spec for: #'common' do: [
		spec blessing: #'bleedingEdge'.
		spec description: 'Bleeding edge of everything'.
		 
		spec
			project: 'Athens' with: '2.5';
			project: 'Mocketry default' with: '2.2';
			project: 'Mocketry Tests' with: '2.2'
	].

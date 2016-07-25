versions
version14: spec
	<version: '1.4' imports: #('1.4-baseline') >

	spec for: #'common' do: [
		spec description: 'A lot of visual improvements and multiple fixes.'.
		spec author: 'MartinDias'.
		spec timestamp: '22/10/2013'.
		spec 
			package: 'Ombu' with: 'Ombu-MartinDias.110';
			package: 'Epicea' with: 'Epicea-MartinDias.329'. ].
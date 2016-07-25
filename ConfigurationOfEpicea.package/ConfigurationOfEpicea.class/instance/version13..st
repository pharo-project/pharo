versions
version13: spec
	<version: '1.3' imports: #('1.3-baseline') >

	spec for: #'common' do: [
		spec description: 'Support STON persistence and other improvements.'.
		spec author: 'MartinDias'.
		spec timestamp: '25/09/2013'.
		spec project: 'Phexample' with: '1.2'.
		spec 
			package: 'Ombu' with: 'Ombu-MartinDias.106';
			package: 'Epicea' with: 'Epicea-MartinDias.256'. ].
versions
version29: spec
	<version: '2.9' imports: #('2.0-baseline') >

	spec for: #'common' do: [
		spec description: '
			Fixed 2 failing tests. Now everything is green :)
			'.
		spec author: 'MartinDias'.
		spec timestamp: '19/02/2014'.
		spec 
			package: 'Ombu' with: 'Ombu-MartinDias.128';
			package: 'Epicea' with: 'Epicea-MartinDias.514'.
			
		spec
			preLoadDoIt: #version23PreLoad;
			postLoadDoIt: #version21PostLoad ].
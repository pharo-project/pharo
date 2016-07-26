versions
version30: spec
	<version: '3.0' imports: #('2.0-baseline') >

	spec for: #'common' do: [
		spec description: '
			- Update for Pharo 30777.
			- Increased browser initial #pageSize from 20 to 30.
			'.
		spec author: 'MartinDias'.
		spec timestamp: '21/02/2014'.
		spec 
			package: 'Ombu' with: 'Ombu-MartinDias.128';
			package: 'Epicea' with: 'Epicea-MartinDias.516'.
			
		spec
			preLoadDoIt: #version23PreLoad;
			postLoadDoIt: #version21PostLoad ].
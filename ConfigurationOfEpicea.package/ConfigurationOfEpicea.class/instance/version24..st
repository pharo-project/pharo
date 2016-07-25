versions
version24: spec
	<version: '2.4' imports: #('2.0-baseline') >

	spec for: #'common' do: [
		spec description: '
			- Log test suite runs.
			- Fix bug when Author is not set.
			- Fix/workaround for bug in class modification.
			- Added a new view for EpLogBrowser.
			- Show ombu filename in EpLogBrowser.
			'.
		spec author: 'MartinDias'.
		spec timestamp: '13/02/2014'.
		spec 
			package: 'Ombu' with: 'Ombu-MartinDias.125';
			package: 'Epicea' with: 'Epicea-MartinDias.494'.
			
		spec
			preLoadDoIt: #version23PreLoad;
			postLoadDoIt: #version21PostLoad ].
versions
version20: spec
	<version: '2.0' imports: #('2.0-baseline') >

	spec for: #'common' do: [
		spec description: '
			- Ready for preliminar study.
			- New: Sessions Browser.
			- Working in Pharo #30753.
			'.
		spec author: 'MartinDias'.
		spec timestamp: '10/02/2014'.
		spec 
			package: 'Ombu' with: 'Ombu-MartinDias.125';
			package: 'Epicea' with: 'Epicea-MartinDias.483'.
			
		spec
			preLoadDoIt: #version20PreLoad;
			postLoadDoIt: #version20PostLoad ].
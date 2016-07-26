versions
version25: spec
	<version: '2.5' imports: #('2.0-baseline') >

	spec for: #'common' do: [
		spec description: '
			- Ombu: Fix bug while parsing files.
			- Ombu: Tolerate Ston parsing errors.
			- Some visual changes on Log Browser and Session Browser.
			'.
		spec author: 'MartinDias'.
		spec timestamp: '14/02/2014'.
		spec 
			package: 'Ombu' with: 'Ombu-MartinDias.127';
			package: 'Epicea' with: 'Epicea-MartinDias.504'.
			
		spec
			preLoadDoIt: #version23PreLoad;
			postLoadDoIt: #version21PostLoad ].
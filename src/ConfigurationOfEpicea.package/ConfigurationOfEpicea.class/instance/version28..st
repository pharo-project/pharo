versions
version28: spec
	<version: '2.8' imports: #('2.0-baseline') >

	spec for: #'common' do: [
		spec description: '
			Minor visual improvements in Browser:
			- remember how many pages where fetched.
			- set a title to the items menu
			'.
		spec author: 'MartinDias'.
		spec timestamp: '19/02/2014'.
		spec 
			package: 'Ombu' with: 'Ombu-MartinDias.128';
			package: 'Epicea' with: 'Epicea-MartinDias.513'.
			
		spec
			preLoadDoIt: #version23PreLoad;
			postLoadDoIt: #version21PostLoad ].
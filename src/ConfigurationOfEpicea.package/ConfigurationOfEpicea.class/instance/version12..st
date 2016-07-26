versions
version12: spec
	<version: '1.2' imports: #('1.0-baseline') >

	spec for: #'common' do: [
		spec description: 'Version showed at ESUG Innovation Technology Awards. Image used: Pharo-30381.'.
		spec author: 'MartinDias'.
		spec timestamp: '9/10/2013 09:57'.
		spec project: 'Phexample' with: '1.2'.
		spec 
			package: 'Ombu' with: 'Ombu-MartinDias.96';
			package: 'Epicea' with: 'Epicea-MartinDias.229'. ].
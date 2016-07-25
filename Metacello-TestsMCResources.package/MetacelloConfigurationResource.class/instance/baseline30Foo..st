ConfigurationOfFoo
baseline30Foo: spec 
	"self reset"
	<version: '3.0'>
	
	spec for: #common do: [
		spec repository: 'dictionary://Metacello_Gofer_Test_Repository'.
		spec 
			package: 'GoferFoo' with: [
				spec 
					preLoadDoIt: #preloadDoIt;
					file: 'GoferFoo-lr.1' ];
			package: 'GoferBar' with: 'GoferBar.branch-lr.1';
			package: 'GoferFaux' with: 'GoferFaux-tg.30';
			package: 'GoferBeau' with: 'GoferBeau-dkh.53';
			yourself ].

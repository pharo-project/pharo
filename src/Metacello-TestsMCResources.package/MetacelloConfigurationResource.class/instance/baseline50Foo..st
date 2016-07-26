ConfigurationOfFoo
baseline50Foo: spec 
	"self reset"
	<version: '5.0'>
	
	spec for: #common do: [
		spec repository: 'dictionary://Metacello_Gofer_Test_Repository'.
		spec 
			package: 'GoferFoo' with:'GoferFoo-lr.2';
			package: 'GoferBar' with: 'GoferBar-lr.1';
			package: 'GoferFaux' with: 'GoferFaux-tg.34';
			package: 'GoferBeau' with: 'GoferBeau-dkh.55';
			yourself.
		spec
			group: '1' with: #('GoferFoo' 'GoferBar');
			group: '2' with: #('GoferFoo' 'GoferFaux');
			yourself ].

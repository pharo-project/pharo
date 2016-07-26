ConfigurationOfFoo
baseline20Foo: spec 
	"self reset"
	<version: '2.0'>
	
	spec for: #common do: [
		spec repository: 'dictionary://Metacello_Gofer_Test_Repository'.
		spec 
			package: 'GoferFoo' with: 'GoferFoo-lr.1';
			package: 'GoferBar' with: 'GoferBar.branch-lr.1';
			package: 'GoferFaux' with: 'GoferFaux-tg.30';
			package: 'GoferBeau' with: 'GoferBeau-dkh.25';
			yourself ].

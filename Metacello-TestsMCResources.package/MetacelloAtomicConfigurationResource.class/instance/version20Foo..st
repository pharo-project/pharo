ConfigurationOfAtomicFoo
version20Foo: spec 
	"self reset"
	<version: '2.0' imports: #('2.0-baseline')>
	
	spec for: #common do: [
		spec repository: 'dictionary://Metacello_Gofer_Test_Repository'.
		spec blessing: #release.
		spec 
			package: 'GoferBar' with: 'GoferBar-dkh.1';
			package: 'GoferFoo' with: 'GoferFoo-dkh.4';
			package: 'GoferBeau' with: 'GoferBeau-dkh.15';
			package: 'GoferFaux' with: 'GoferFaux-tg.30';
			yourself ].

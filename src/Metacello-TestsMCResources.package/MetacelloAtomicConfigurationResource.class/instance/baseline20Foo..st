ConfigurationOfAtomicFoo
baseline20Foo: spec 
	"self reset"
	<version: '2.0-baseline'>
	
	spec for: #common do: [
		spec repository: 'dictionary://Metacello_Gofer_Test_Repository'.
		spec blessing: #baseline.
		spec 
			package: 'GoferBar';
			package: 'GoferFoo';
			package: 'GoferBeau';
			package: 'GoferFaux';
			yourself ].

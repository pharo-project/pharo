ConfigurationOfAtomicFoo
version51Foo: spec 
	"self reset"
	<version: '5.1'>
	
	spec for: #common do: [
		spec repository: 'dictionary://Metacello_Gofer_Test_Repository'.
		spec blessing: #release.
		spec 
			package: 'GoferUmbrella' with: 'GoferUmbrella-lr.5';
			yourself ].

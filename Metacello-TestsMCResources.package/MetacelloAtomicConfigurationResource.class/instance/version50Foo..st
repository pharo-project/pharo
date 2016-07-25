ConfigurationOfAtomicFoo
version50Foo: spec 
	"self reset"
	<version: '5.0'>
	
	spec for: #common do: [
		spec repository: 'dictionary://Metacello_Gofer_Test_Repository'.
		spec blessing: #release.
		spec 
			package: 'GoferUmbrella' with: 'GoferUmbrella-dkh.4';
			yourself ].

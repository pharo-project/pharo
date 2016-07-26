ConfigurationOfAtomicFoo
baseline25Foo: spec 
	"self reset"
	<version: '2.5-baseline'>
	
	spec for: #common do: [
		spec repository: 'dictionary://Metacello_Gofer_Test_Repository'.
		spec blessing: #baseline.
		spec 
			package: 'GoferFoo' with: [spec requires: 'GoferFan' ];
			yourself.
		spec 
			project: 'GoferFan' with: [ "requires GoferBeau"
				spec
					className: 'MetacelloTestConfigurationOfAtomicFan';
					versionString: '2.0-baseline';
					loads: 'GoferFan';
					repository: 'dictionary://Metacello_Configuration_Test_Repository' ];
			yourself ].

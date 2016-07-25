ConfigurationOfFan
baseline35Fan: spec 
	"self reset"
	<version: '3.5'>
	
	spec for: #common do: [
		spec repository: 'dictionary://Metacello_Gofer_Test_Repository'.
		spec 
			project: 'Foo' with: [
				spec
					className: 'MetacelloTestConfigurationOfFoo';
					versionString: '3.5';
					loads: #('GoferFaux' 'GoferBeau');
					file: 'MetacelloTestConfigurationOfFoo';
					repository: 'dictionary://Metacello_Configuration_Test_Repository' ];
			yourself ].

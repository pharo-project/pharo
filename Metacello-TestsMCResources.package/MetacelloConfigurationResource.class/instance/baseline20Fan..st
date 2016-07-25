ConfigurationOfFan
baseline20Fan: spec 
	"self reset"
	<version: '2.0'>
	
	spec for: #common do: [
		spec repository: 'dictionary://Metacello_Gofer_Test_Repository'.
		spec 
			project: 'Foo' with: [
				spec
					className: 'MetacelloTestConfigurationOfFoo';
					versionString: '2.0';
					loads: #('GoferFaux' 'GoferBeau');
					file: 'MetacelloTestConfigurationOfFoo';
					repository: 'dictionary://Metacello_Configuration_Test_Repository' ];
			project: 'Far' with: [
				spec
					className: 'MetacelloTestConfigurationOfFoo';
					versionString: '2.0';
					loads: #('GoferBeau');
					file: 'MetacelloTestConfigurationOfFoo';
					repository: 'dictionary://Metacello_Configuration_Test_Repository' ];
			yourself ].

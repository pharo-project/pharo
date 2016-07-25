ConfigurationOfUmbrella
version50Umbrella: spec 
	"self reset"
	<version: '5.0'>
	
	spec for: #common do: [
		spec 
			project: 'Umbrella' with: [
				spec
					className: 'MetacelloTestConfigurationOfLinearFoo';
					versionString: '5.0';
					repository: 'dictionary://Metacello_Configuration_Test_Repository' ];
			yourself ].

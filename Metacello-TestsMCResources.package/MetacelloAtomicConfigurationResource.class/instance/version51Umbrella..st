ConfigurationOfUmbrella
version51Umbrella: spec 
	"self reset"
	<version: '5.1'>
	
	spec for: #common do: [
		spec 
			project: 'Umbrella' with: [
				spec
					className: 'MetacelloTestConfigurationOfLinearFoo';
					versionString: '5.1';
					repository: 'dictionary://Metacello_Configuration_Test_Repository' ];
			yourself ].

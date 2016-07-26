ConfigurationOfProjectFee
baseline40ProjectFee: spec 
	"projects and packages"
	"self reset"
	<version: '4.0'>
	
	spec for: #common do: [
		spec repository: 'dictionary://Metacello_Gofer_Test_Repository'.
		spec 
			project: 'Foo' with: [
				spec
					className: 'MetacelloTestConfigurationOfFoo';
					versionString: '4.0';
					file: 'MetacelloTestConfigurationOfFoo';
					repository: 'dictionary://Metacello_Configuration_Test_Repository' ];
			yourself.
		spec 
			package: 'GeauxFaux' with: 'GeauxFaux-tg.32';
			package: 'GeauxBeau' with: 'GeauxBeau-dkh.55';
			yourself ].

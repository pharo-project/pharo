ConfigurationOfProjectFie
baseline35ProjectFie: spec 
	"just projects"
	"self reset"

	<version: '3.5'>
	spec for: #common do: [
		spec 
			project: 'Foo' with: [
				spec
					className: 'MetacelloTestConfigurationOfFoo';
					versionString: '3.5';
					file: 'MetacelloTestConfigurationOfFoo';
					repository: 'dictionary://Metacello_Configuration_Test_Repository' ];
			project: 'Feaux' with: [
				spec
					className: 'MetacelloTestConfigurationOfFeaux';
					versionString: '4.0';
					file: 'MetacelloTestConfigurationOfFeaux';
					repository: 'dictionary://Metacello_Configuration_Test_Repository' ];
			yourself. ].

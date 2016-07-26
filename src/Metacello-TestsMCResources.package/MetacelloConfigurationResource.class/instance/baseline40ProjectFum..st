ConfigurationOfFum
baseline40ProjectFum: spec 
	"master"
	"self reset"
	<version: '4.0'>
	
	spec for: #common do: [
		spec repository: 'dictionary://Metacello_Gofer_Test_Repository'.
		spec 
			project: 'Feaux' with: [
				spec
					className: 'MetacelloTestConfigurationOfFeaux';
					versionString: '4.0';
					file: 'MetacelloTestConfigurationOfFeaux';
					repository: 'dictionary://Metacello_Configuration_Test_Repository' ];
			project: 'Foo' with: [
				spec
					className: 'MetacelloTestConfigurationOfFoo';
					versionString: '4.0';
					file: 'MetacelloTestConfigurationOfFoo';
					repository: 'dictionary://Metacello_Configuration_Test_Repository' ];
			project: 'ProjectFie' with: [
				spec
					className: 'MetacelloTestConfigurationOfProjectFie';
					versionString: '4.0';
					file: 'MetacelloTestConfigurationOfProjectFie';
					repository: 'dictionary://Metacello_Configuration_Test_Repository' ];
			project: 'ProjectFee' with: [
				spec
					className: 'MetacelloTestConfigurationOfProjectFee';
					versionString: '4.0';
					file: 'MetacelloTestConfigurationOfProjectFee';
					repository: 'dictionary://Metacello_Configuration_Test_Repository' ];
			yourself ].

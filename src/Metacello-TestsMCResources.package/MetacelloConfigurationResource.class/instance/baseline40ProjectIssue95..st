Issue 95
baseline40ProjectIssue95: spec 
	"self reset"
	<version: '4.0'>
	
	spec for: #common do: [
		spec repository: 'dictionary://Metacello_Gofer_Test_Repository'.
		spec 
			project: 'Loop' with: [
				spec
					className: 'MetacelloTestConfigurationOfProjectLoop';
					versionString: '4.0';
					loads: 'GoferFaux';
					repository: 'dictionary://Metacello_Configuration_Test_Repository' ];
			project: 'Infinite' with: [
				spec
					className: 'MetacelloTestConfigurationOfProjectInfinite';
					versionString: '4.0';
					loads: 'GeauxBar';
					repository: 'dictionary://Metacello_Configuration_Test_Repository' ];
			yourself ].
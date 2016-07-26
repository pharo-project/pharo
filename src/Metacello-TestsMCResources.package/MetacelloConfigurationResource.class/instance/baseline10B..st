Issue 77
baseline10B: spec 
	"self reset"
	<version: '1.0'>
	
	spec for: #common do: [
		spec repository: 'dictionary://Metacello_Gofer_Test_Repository'.
		spec 
			project: 'C' with: [
				spec
					className: 'MetacelloTestConfigurationOfIssue77C';
					loads: #('GoferFoo');
					versionString: '1.2';
					repository: 'dictionary://Metacello_Configuration_Test_Repository' ];
			package: 'GoferBeau' with: 'GoferBeau-dkh.53';
			yourself ].

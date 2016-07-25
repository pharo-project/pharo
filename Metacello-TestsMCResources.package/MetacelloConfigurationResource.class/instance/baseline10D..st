Issue 77
baseline10D: spec 
	"self reset"
	<version: '1.0'>
	
	spec for: #common do: [
		spec repository: 'dictionary://Metacello_Gofer_Test_Repository'.
		spec 
			project: 'C' with: [
				spec
					className: 'MetacelloTestConfigurationOfIssue77C';
					loads: #('GoferFoo');
					versionString: '1.3';
					repository: 'dictionary://Metacello_Configuration_Test_Repository' ];
			package: 'GoferBar' with: [
				spec 
					file:'GoferBar-lr.1' ];
			group: 'default' with: #('GoferBar');
			yourself ].

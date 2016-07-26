Issue 86
baseline61ProjectIssue86: spec 
	"projects and packages"
	"self reset"
	<version: '6.1'>
	
	spec for: #common do: [
		spec repository: 'dictionary://Metacello_Gofer_Test_Repository'.
		spec 
			project: 'A' with: [
				spec
					className: 'MetacelloTestConfigurationOfLinearFoo';
					versionString: '6.1';
					loads: #('1');
					repository: 'dictionary://Metacello_Configuration_Test_Repository' ];
			project: 'B' with: [
				spec
					className: 'MetacelloTestConfigurationOfLinearFoo';
					versionString: '6.1';
					loads: #('2');
					repository: 'dictionary://Metacello_Configuration_Test_Repository' ];
			yourself ].

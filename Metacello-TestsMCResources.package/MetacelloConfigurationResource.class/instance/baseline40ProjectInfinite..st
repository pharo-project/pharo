Issue 95
baseline40ProjectInfinite: spec 
	"self reset"
	<version: '4.0'>
	
	spec for: #common do: [
		spec repository: 'dictionary://Metacello_Gofer_Test_Repository'.
		spec
			project: 'Loop' with: [
				spec
					className: 'MetacelloTestConfigurationOfProjectLoop';
					versionString: '4.0';
					loads: 'GeauxBeau';
					repository: 'dictionary://Metacello_Configuration_Test_Repository' ];
			yourself.
		spec 
			package: 'GeauxBar' with: [
				spec
					file: 'GeauxBar.branch-lr.2';
					requires: 'Loop';
					yourself ];
			package: 'GoferBar' with: 'GoferBar-lr.1';
			yourself ].
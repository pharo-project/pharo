Issue 95
baseline40ProjectLoop: spec 
	"self reset"
	<version: '4.0'>
	
	spec for: #common do: [
		spec repository: 'dictionary://Metacello_Gofer_Test_Repository'.
		spec 
			project: 'Infinite' with: [
				spec
					className: 'MetacelloTestConfigurationOfProjectInfinite';
					versionString: '4.0';
					loads: 'GoferBar';
					repository: 'dictionary://Metacello_Configuration_Test_Repository' ];
			package: 'GoferFaux' with: [
				spec
					file:'GoferFaux-tg.30';
					requires: 'Infinite';
					yourself ];
			package: 'GeauxBeau' with: [
				spec
					file: 'GeauxBeau-dkh.55';
					yourself ];
			yourself ].

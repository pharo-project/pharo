ConfigurationOfAtomicFan
baseline31Fan: spec 
	"self reset"
	<version: '3.1-baseline'>
	
	spec for: #common do: [
		spec repository: 'dictionary://Metacello_Gofer_Test_Repository'.
		spec postLoadDoIt: #postLoad31baseline.
		spec 
			package: 'GoferBar';
			package: 'GoferBeau' with: [ 
				spec 
					requires: 'GoferBar';
					includes: #('GoferFoo' ) ]; 
			package: 'GoferFar' with: [ spec requires: #('GoferFoo' ) ]; 
			yourself.
		spec 
			project: 'GoferFoo' with: [ "requires GoferBeau"
				spec
					className: 'MetacelloTestConfigurationOfLinearFoo';
					versionString: '2.0';
					loads: 'GoferFoo';
					repository: 'dictionary://Metacello_Configuration_Test_Repository' ];
			yourself ].

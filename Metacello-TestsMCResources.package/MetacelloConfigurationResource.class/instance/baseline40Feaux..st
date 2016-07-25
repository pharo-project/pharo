ConfigurationOfFeaux
baseline40Feaux: spec 
	"self reset"
	<version: '4.0'>
	
	spec for: #common do: [
		spec repository: 'dictionary://Metacello_Gofer_Test_Repository'.
		spec 
			package: 'GeauxFoo' with:'GeauxFoo-lr.1';
			package: 'GeauxBar' with: 'GeauxBar.branch-lr.2';
			yourself ].

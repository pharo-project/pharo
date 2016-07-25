Issue 77
baseline13C: spec 
	"self reset"
	<version: '1.3'>
	
	spec for: #common do: [
		spec repository: 'dictionary://Metacello_Gofer_Test_Repository'.
		spec 
			package: 'GoferFoo' with: 'GoferFoo-lr.4';
			package: 'GeauxBeau' with: 'GeauxBeau-dkh.56';
			yourself ].

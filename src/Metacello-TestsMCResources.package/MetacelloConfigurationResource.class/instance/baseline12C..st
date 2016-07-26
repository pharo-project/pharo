Issue 77
baseline12C: spec 
	"self reset"
	<version: '1.2'>
	
	spec for: #common do: [
		spec repository: 'dictionary://Metacello_Gofer_Test_Repository'.
		spec 
			package: 'GoferFoo' with: 'GoferFoo-lr.1';
			package: 'GeauxBeau' with: 'GeauxBeau-dkh.55';
			yourself. ].

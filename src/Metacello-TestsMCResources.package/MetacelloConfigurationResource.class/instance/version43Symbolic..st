ConfigurationOfSymbolic
version43Symbolic: spec 
	"self reset"
	<version: '4.3'>
	
	spec for: #common do: [
		spec repository: 'dictionary://Metacello_Gofer_Test_Repository'.
		spec blessing: #development.
		spec 
			package: 'GoferBeau' with: 'GoferBeau-dkh.54';
			package: 'GoferFaux' with:'GoferFaux-tg.33';
			yourself ].

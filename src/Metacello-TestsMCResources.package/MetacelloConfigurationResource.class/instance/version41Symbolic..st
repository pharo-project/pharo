ConfigurationOfSymbolic
version41Symbolic: spec 
	"self reset"
	<version: '4.1'>
	
	spec for: #common do: [
		spec repository: 'dictionary://Metacello_Gofer_Test_Repository'.
		spec blessing: #release.
		spec 
			package: 'GoferBeau' with: 'GoferBeau-dkh.25';
			package: 'GoferFaux' with:'GoferFaux-tg.31';
			yourself ].

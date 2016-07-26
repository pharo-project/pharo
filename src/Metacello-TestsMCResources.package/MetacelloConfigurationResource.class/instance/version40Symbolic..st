ConfigurationOfSymbolic
version40Symbolic: spec 
	"self reset"
	<version: '4.0'>
	
	spec for: #common do: [
		spec repository: 'dictionary://Metacello_Gofer_Test_Repository'.
		spec blessing: #release.
		spec 
			package: 'GoferBeau' with: 'GoferBeau-dkh.15';
			package: 'GoferFaux' with:'GoferFaux-tg.30';
			yourself ].

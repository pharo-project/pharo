ConfigurationOfFix
baseline61Fix: spec 
	"self reset"
	<version: '6.1'>
	
	spec for: #common do: [
		spec repository: 'dictionary://Metacello_Gofer_Test_Repository'.
		spec 
			package: 'GoferFoo' with: 'GoferFoo-lr.4';
			package: 'GoferBar' with: 'GoferBar-lr.1';
			package: 'GoferBeau' with: 'GoferBeau-dkh.53';
			package: 'GoferFaux' with: 'GoferFaux-tg.31';
			yourself. ].

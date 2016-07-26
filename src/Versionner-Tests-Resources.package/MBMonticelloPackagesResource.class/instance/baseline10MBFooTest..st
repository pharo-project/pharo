ConfigurationOfMBFooTest
baseline10MBFooTest: spec 
	"self reset"
	<version: '1.0-baseline'>
	
	spec for: #common do: [
		spec repository: 'dictionary://MB_Foo_Test_Repository'.
		spec blessing: #baseline.
		spec
			package: 'MBFooTests';
			yourself ].

command tests
baseline30MBFooTest: spec 
	"self reset"
	<version: '3.0-baseline'>
	
	spec for: #common do: [
		spec repository: 'dictionary://MB_Foo_Test_Repository'.
		spec blessing: #baseline.
		spec
			package: 'MBFooTests';
			yourself ].

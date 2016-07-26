command tests
symbolicVersionDevelopment10MBFooTest: spec
	"self reset"
	<symbolicVersion: #'development'>

	spec for: #'common' version: '1.0'.
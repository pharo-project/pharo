command tests
symbolicVersionBleedingEdge20baselineMBFooTest: spec
	"self reset"
	<symbolicVersion: #'bleedingEdge'>

	spec for: #'common' version: '2.0-baseline'.
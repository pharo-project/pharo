array arithmetic primitives
primSubScalar: rcvr and: other into: result

	<primitive: 'primitiveSubScalar' module:'KedamaPlugin'>
	"^ KedamaPlugin doPrimitive: #primitiveSubScalar."

	1 to: rcvr size do: [:i |
		result at: i put: (rcvr at: i) - other.
	].
	^ result.

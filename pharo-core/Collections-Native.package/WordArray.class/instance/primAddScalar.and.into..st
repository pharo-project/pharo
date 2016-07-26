array arithmetic primitives
primAddScalar: rcvr and: other into: result

	<primitive: 'primitiveAddScalar' module:'KedamaPlugin'>
	"^ KedamaPlugin doPrimitive: #primitiveAddScalar."

	1 to: rcvr size do: [:i |
		result at: i put: (rcvr at: i) + other.
	].
	^ result.

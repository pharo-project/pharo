array arithmetic primitives
primMulScalar: rcvr and: other into: result

	<primitive: 'primitiveMulScalar' module:'KedamaPlugin'>
	"^ KedamaPlugin doPrimitive: #primitiveMulScalar."

	1 to: rcvr size do: [:i |
		result at: i put: (rcvr at: i) * other.
	].
	^ result.

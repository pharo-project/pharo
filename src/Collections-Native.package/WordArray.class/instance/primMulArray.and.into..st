array arithmetic primitives
primMulArray: rcvr and: other into: result

	<primitive: 'primitiveMulArrays' module:'KedamaPlugin'>
	"^ KedamaPlugin doPrimitive: #primitiveMulArrays."

	1 to: rcvr size do: [:i |
		result at: i put: (rcvr at: i) * (other at: i)
	].
	^ result.

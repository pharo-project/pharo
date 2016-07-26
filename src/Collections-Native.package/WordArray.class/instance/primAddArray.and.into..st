array arithmetic primitives
primAddArray: rcvr and: other into: result

	<primitive: 'primitiveAddArrays' module:'KedamaPlugin'>
	"^ KedamaPlugin doPrimitive: #primitiveAddArrays."

	1 to: rcvr size do: [:i |
		result at: i put: (rcvr at: i) + (other at: i)
	].
	^ result.

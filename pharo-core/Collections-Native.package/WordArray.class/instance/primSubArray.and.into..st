array arithmetic primitives
primSubArray: rcvr and: other into: result

	<primitive: 'primitiveSubArrays' module:'KedamaPlugin'>
	"^ KedamaPlugin doPrimitive: #primitiveSubArrays."

	1 to: rcvr size do: [:i |
		result at: i put: (rcvr at: i) - (other at: i)
	].
	^ result.

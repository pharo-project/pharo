array arithmetic primitives
primDivArray: rcvr and: other into: result

	<primitive: 'primitiveDivArrays' module:'KedamaPlugin'>
	"^ KedamaPlugin doPrimitive: #primitiveDivArrays."

	1 to: rcvr size do: [:i |
		result at: i put: (rcvr at: i) / (other at: i)
	].
	^ result.

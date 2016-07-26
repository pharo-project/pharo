array arithmetic primitives
primDivScalar: rcvr and: other into: result

	<primitive: 'primitiveDivScalar' module:'KedamaPlugin'>
	"^ KedamaPlugin doPrimitive: #primitiveDivScalar."

	1 to: rcvr size do: [:i |
		result at: i put: (rcvr at: i) / other.
	].
	^ result.

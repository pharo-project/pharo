primitives-plugin
primDivScalar: scalarValue

	<primitive: 'primitiveDivScalar' module: 'FloatArrayPlugin'>
	1 to: self size do:[:i| self at: i put: (self at: i) / scalarValue].
primitives-plugin
primMulScalar: scalarValue

	<primitive: 'primitiveMulScalar' module: 'FloatArrayPlugin'>
	1 to: self size do:[:i| self at: i put: (self at: i) * scalarValue].
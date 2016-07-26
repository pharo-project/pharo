primitives-plugin
primAddArray: floatArray

	<primitive: 'primitiveAddFloatArray' module: 'FloatArrayPlugin'>
	1 to: self size do:[:i| self at: i put: (self at: i) + (floatArray at: i)].
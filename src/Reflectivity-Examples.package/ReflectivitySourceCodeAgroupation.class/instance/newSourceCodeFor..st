instance creation
newSourceCodeFor: aCompiledMethod
	self addMorph: (ReflectivityCodeMorph compiledMethod: aCompiledMethod)
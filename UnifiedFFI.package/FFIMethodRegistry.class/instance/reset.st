accessing
reset
	"FFI compiled methods will keep the old method as a property, making easy to replace it 
	 when changing platforms."
	self compiledMethods do: [ :each |
		each methodClass methodDict 
			at: each selector 
			put: (each propertyValueAt: #ffiNonCompiledMethod)].
	self compiledMethods removeAll
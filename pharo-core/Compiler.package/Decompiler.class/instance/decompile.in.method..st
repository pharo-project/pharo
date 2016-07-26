private
decompile: aSelector in: aClass method: aMethod

	^self
		decompile: aSelector
		in: aClass
		method: aMethod
		using: (self constructorForMethod: aMethod)
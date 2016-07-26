public access
decompileMethod: aMethod

	^self
		decompile: aMethod selector
		in: aMethod methodClass
		method: aMethod
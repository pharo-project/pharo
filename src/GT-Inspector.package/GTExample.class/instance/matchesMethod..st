testing
matchesMethod: aCompiledMethod
	^ aCompiledMethod class = CompiledMethod
		and: [ self method methodClass = aCompiledMethod methodClass
			and: [ self method selector = aCompiledMethod selector ] ]
private
proxy: anExampleProxy matchesMethod: aCompiledMethod
	^ aCompiledMethod class = CompiledMethod
		and: [ anExampleProxy provider class = aCompiledMethod methodClass
			and: [ anExampleProxy selector = aCompiledMethod selector ] ]
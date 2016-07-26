private - actions
runScriptForPragma: pragma

	self class compiler
			evaluate: pragma arguments first
			for: method methodClass theNonMetaClass
			logged: false

private - actions
runScript
	method pragmas
		select: [ :pragma | pragma keyword = self pragmaName ]
		thenDo: [ :pragma | 
			self runScriptForPragma: pragma.
			UIManager default inform: 'Script executed: ' , method name asString ]
		
	 
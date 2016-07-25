generate scripts/methods
compileScriptMethodWithCurrentPackages: aNumber 
	"ScriptLoader new compileScriptMethodWithCurrentPackages: 9999"
	
	self class compile: 
		(self newGenerateScriptTemplateWithCurrentPackages: aNumber)
		classified: 'pharo - scripts'
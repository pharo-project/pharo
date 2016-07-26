ci
generateScriptAndUpdateMethodForNewVersionSilentlyConfigurations: configurations preLoad: preload postLoad: postLoad
	
	"remove old update and script methods"
	self removeUpdateMethods.
	self removeScriptMethods.
	
	self compileScriptMethodWithCurrentPackages: self currentScriptVersionNumber.
	self 
		compileNewUpdateMethodSilentlyConfigurations: configurations 
		preLoad: preload 
		postLoad: postLoad
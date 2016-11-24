elementary steps
generateScriptAndUpdateMethodForNewVersion
	"Use me to generate the script and update method"
	"self new generateScriptAndUpdateMethodForNewVersion"
	
	"remove old update and script methods"
	self removeUpdateMethods.
	self removeScriptMethods.
	
	self compileScriptMethodWithCurrentPackages: self currentScriptVersionNumber.
	self compileNewUpdateMethod.
	

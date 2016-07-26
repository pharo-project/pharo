defaults
defaultPath
	"Returns the default path on the update server. For instance 'pharo4.0' 
 	 within http://updates.pharo.org/pharo4.0/"
	
	^'pharo', (Smalltalk globals at: #ScriptLoader) currentMajorVersionNumber asString  
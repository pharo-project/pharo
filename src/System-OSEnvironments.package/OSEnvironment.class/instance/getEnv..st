accessing
getEnv: varName
	
	"This method calls the Standard C Library getenv() function."
	"OSEnvironment current getEnv: 'HOME' "
	
	<primitive: 'primitiveGetenv' module: '' error: ec>
	ec ifNil: [ ^self getEnvViaFFI: varName ].
	ec == #'bad argument' ifTrue: [
		varName isString 
			ifFalse: [ ^self getEnv: varName asString ] ].
	self primitiveFail
I am ZnSignalProgress.
I am a DynamicVariable and a ProcessSpecificVariable.

I can be used to indicate that Zn related code down the call stack has to signal HTTPProgress, for example:

ZnSignalProgress 
	value: true 
	during: [ ^ ZnClient new get: 'http://zn.stfx.eu/zn/small.html' ]
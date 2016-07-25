I am ZnConnectionTimeout.
I am a DynamicVariable and a ProcessSpecificVariable.

I can be used to modify the global default socket IO timeout on a per process basis, for example:

ZnConnectionTimeout 
	value: 5 
	during: [ ^ ZnClient new get: 'http://zn.stfx.eu/zn/small.html' ]
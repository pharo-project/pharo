I am ZnDefaultCharacterEncoder.
I am a DynamicVariable and a ProcessSpecificVariable.

I can be used to modify the default ZnCharacteEncoder on a per process basis, for example:

ZnDefaultCharacterEncoder 
	value: ZnUTF8Encoder latin1
	during: [ ZnClient new get: 'http://zn.stfx.eu/zn/small.html' ]
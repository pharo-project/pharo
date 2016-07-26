initialize
canDecompile: aCompiledMethod
	"Answers false if aCompiledMethod can't be decompiled due to framework specific constraints."
	^ (self isPrimitiveNativeCall: aCompiledMethod) not
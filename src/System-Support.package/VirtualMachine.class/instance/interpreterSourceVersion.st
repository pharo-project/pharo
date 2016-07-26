accessing
interpreterSourceVersion
	"The use of this primitive not recommended. Not all VMs providing that"

	"Answer a string corresponding to the version of the interpreter source.
	This represents the version level of the Smalltalk source code (interpreter
	and various plugins) that is translated to C by a CCodeGenerator, as distinct
	from the external platform source code, typically written in C and managed
	separately for each platform. An optional primitive is invoked that may not
	be available on all virtual machines."

	"Smalltalk vm interpreterSourceVersion"

	<primitive: 'primitiveInterpreterSourceVersion'>
	^ self getSystemAttribute: 1009
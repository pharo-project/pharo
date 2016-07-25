private
removeVariable: aRGVariableDefinition from: aCollection
	"Removes a variable from a particular collection.
	This behavior is the same for any kind of variable"
	aCollection remove: aRGVariableDefinition ifAbsent:[]
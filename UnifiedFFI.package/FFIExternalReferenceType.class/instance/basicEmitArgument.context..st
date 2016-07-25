emitting code
basicEmitArgument: aBuilder context: aContext
 	self loader
		emitArgument: aBuilder 
		context: aContext 
		objectClass: self objectClass	
		instVarName: self instanceVariableName
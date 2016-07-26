*Monticello
asMCMethodDefinitionFromActiveDefinition
	"Creates a MCMethodDefinition from a source file assuming the current method isActive."

	| compiledMethod methodClass|
	compiledMethod := self compiledMethod.
	methodClass := compiledMethod methodClass.
	
   ^ MCMethodDefinition
		className: methodClass theNonMetaClass name
 	   	classIsMeta: methodClass isMeta
		selector: compiledMethod selector
		category: compiledMethod category
		timeStamp: compiledMethod timeStamp
		source: compiledMethod sourceCode
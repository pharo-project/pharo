testing method dictionary
whichSelectorsStoreInto: instVarName 
	"Answer a Set of selectors whose methods access the argument, 
	instVarName, as a named instance variable."
	| instVarIndex |
	instVarIndex := self instVarIndexFor: instVarName ifAbsent: [^#()].
	^ self selectors select: [:sel | (self compiledMethodAt: sel) writesField: instVarIndex]

	"Point whichSelectorsStoreInto: 'x'."
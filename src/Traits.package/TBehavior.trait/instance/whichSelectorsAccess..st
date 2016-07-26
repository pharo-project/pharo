testing method dictionary
whichSelectorsAccess: instVarName 
	"Answer a set of selectors whose methods access the argument, 
	instVarName, as a named instance variable."

	| instVarIndex |
	instVarIndex := self instVarIndexFor: instVarName ifAbsent: [^#()].
	^ self selectors select:  [:sel | 
		((self compiledMethodAt: sel)
			readsField: instVarIndex)
			or: [(self compiledMethodAt: sel) writesField: instVarIndex]]
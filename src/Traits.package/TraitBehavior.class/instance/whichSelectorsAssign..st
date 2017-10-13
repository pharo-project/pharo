queries
whichSelectorsAssign: instVarName 
	"Answer a Set of selectors whose methods store into the argument, 
	instVarName, as a named instance variable."
	^self whichSelectorsStoreInto: instVarName
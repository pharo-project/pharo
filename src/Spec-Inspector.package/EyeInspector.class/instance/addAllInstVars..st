list
addAllInstVars: elements
	"Add AllInstVars to the list of elements you can access in the inspector"

	elements add: (AllInstVarsEyeElement host: self object).
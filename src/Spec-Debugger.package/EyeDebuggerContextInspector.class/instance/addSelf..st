list
addSelf: elements
	"Add self to the list of elements you can access in the inspector with the label thisContext"

	elements add: (ThisContextEyeElement host: self object).
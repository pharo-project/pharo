list
addSelf: elements
	"Add self to the list of elements you can access in the inspector"

	elements add: (SelfEyeElement host: self object).
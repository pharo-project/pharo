list
addVariableFields: elements
	"add the variable fields to the inspector. Shorten the list for very long collection (cf limit1 and limit2), set them so that you can modify and save them"

	self variableFieldsToShow do: [ :index |
		elements add: (IndexedEyeElement host: self object index: index) ]
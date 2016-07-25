list
addVariableFields: elements

	self sortedKeys do: [ :key |
		elements add: (IndexedEyeElement host: self object index: key)]
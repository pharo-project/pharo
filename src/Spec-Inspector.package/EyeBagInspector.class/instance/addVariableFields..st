list
addVariableFields: elements

	self sortedKeys do: [ :key |
		elements add: (BagEyeElement host: self object index: key)]
list
addVariableFields: elements

	self tempNames withIndexDo: [ :name :index |
		elements add: (ContextTempEyeElement host: self object tempName: name tempIndex: index) ]
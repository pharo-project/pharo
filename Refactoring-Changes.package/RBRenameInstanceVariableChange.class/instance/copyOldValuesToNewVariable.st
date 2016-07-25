private
copyOldValuesToNewVariable
	| newIndex oldIndex |
	oldIndex := self changeClass allInstVarNames indexOf: oldName asString.
	newIndex := self changeClass allInstVarNames indexOf: newName asString.
	self changeClass withAllSubclasses do: [ :class |
		class allInstances 
			do: [ :each | each instVarAt: newIndex put: (each instVarAt: oldIndex) ] ]
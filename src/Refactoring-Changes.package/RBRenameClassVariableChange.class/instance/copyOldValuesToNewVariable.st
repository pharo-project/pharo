private
copyOldValuesToNewVariable
	| oldValue |
	oldValue := self changeClass classPool at: oldName ifAbsent: [ nil ].
	self changeClass classPool at: newName asSymbol put: oldValue
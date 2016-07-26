initialize-release
rename: aVarName to: aName in: aClass renameAccessors: aBoolean
	self variable: aVarName class: aClass.
	newName := aName.
	renameAccessors := aBoolean
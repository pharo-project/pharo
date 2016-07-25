instance creation
rename: aVarName to: aName in: aClass renameAccessors: aBoolean
	^self new
		rename: aVarName
		to: aName
		in: aClass
		renameAccessors: aBoolean
instance creation
rename: aVarName to: aName in: aClass
	^self new
		rename: aVarName
		to: aName
		in: aClass
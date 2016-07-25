instance creation
model: aRBSmalltalk rename: aVarName to: aName in: aClass renameAccessors: aBoolean
	^(self new)
		model: aRBSmalltalk;
		rename: aVarName
			to: aName
			in: aClass
			renameAccessors: aBoolean;
		yourself
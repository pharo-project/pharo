instance creation
model: aRBSmalltalk rename: aVarName to: aName in: aClass 
	^(self new)
		model: aRBSmalltalk;
		rename: aVarName
			to: aName
			in: aClass;
		yourself
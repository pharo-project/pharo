instance creation
model: aRBSmalltalk renameMethod: aSelector in: aClass to: newSelector permutation: aMap 
	^(self new)
		model: aRBSmalltalk;
		renameMethod: aSelector
			in: aClass
			to: newSelector
			permutation: aMap;
		yourself
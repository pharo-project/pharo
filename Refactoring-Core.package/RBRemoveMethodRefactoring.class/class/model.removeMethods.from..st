instance creation
model: aRBSmalltalk removeMethods: selectorCollection from: aClass 
	^(self new)
		model: aRBSmalltalk;
		removeMethods: selectorCollection from: aClass;
		yourself
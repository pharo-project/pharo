instance creation
model: aRBSmalltalk pushDown: selectorCollection from: aClass 
	^(self new)
		model: aRBSmalltalk;
		pushDown: selectorCollection from: aClass;
		yourself
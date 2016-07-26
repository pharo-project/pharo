instance creation
model: aRBSmalltalk sendersOf: aSelector in: aClass 
	^(self new)
		model: aRBSmalltalk;
		sendersOf: aSelector in: aClass;
		yourself
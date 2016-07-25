instance creation
model: aRBSmalltalk extract: anInterval from: aSelector in: aClass 
	^(self new)
		model: aRBSmalltalk;
		extract: anInterval
			from: aSelector
			in: aClass;
		yourself
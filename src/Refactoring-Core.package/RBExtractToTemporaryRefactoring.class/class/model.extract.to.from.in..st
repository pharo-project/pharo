instance creation
model: aRBSmalltalk extract: anInterval to: aString from: aSelector in: aClass 
	^(self new)
		model: aRBSmalltalk;
		extract: anInterval
			to: aString
			from: aSelector
			in: aClass;
		yourself
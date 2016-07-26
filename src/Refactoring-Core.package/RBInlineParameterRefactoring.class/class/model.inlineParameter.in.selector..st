instance creation
model: aRBSmalltalk inlineParameter: aString in: aClass selector: aSelector 
	^(self new)
		model: aRBSmalltalk;
		inlineParameter: aString
			in: aClass
			selector: aSelector;
		yourself
instance creation
model: aRBSmalltalk removeParameter: aString in: aClass selector: aSelector 
	^(self new)
		model: aRBSmalltalk;
		removeParameter: aString
			in: aClass
			selector: aSelector;
		yourself
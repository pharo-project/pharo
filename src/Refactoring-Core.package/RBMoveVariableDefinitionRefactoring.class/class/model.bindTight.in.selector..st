instance creation
model: aRBSmalltalk bindTight: anInterval in: aClass selector: aSelector 
	^(self new)
		model: aRBSmalltalk;
		class: aClass
			selector: aSelector
			interval: anInterval;
		yourself
instance creation
bindTight: anInterval in: aClass selector: aSelector 
	^self new
		class: aClass
		selector: aSelector
		interval: anInterval
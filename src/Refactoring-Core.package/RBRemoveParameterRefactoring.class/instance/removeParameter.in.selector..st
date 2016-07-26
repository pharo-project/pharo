initialize-release
removeParameter: aString in: aClass selector: aSelector 
	oldSelector := aSelector.
	class := self classObjectFor: aClass.
	argument := aString
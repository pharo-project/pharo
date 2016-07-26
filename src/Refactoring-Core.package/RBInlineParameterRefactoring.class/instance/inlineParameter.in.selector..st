initialize-release
inlineParameter: aString in: aClass selector: aSelector
	oldSelector := aSelector.
	class := self classObjectFor: aClass.
	argument := aString
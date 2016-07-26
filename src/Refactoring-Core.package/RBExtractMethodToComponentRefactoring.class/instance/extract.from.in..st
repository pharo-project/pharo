initialize-release
extract: anInterval from: aSelector in: aClass 
	class := self classObjectFor: aClass.
	selector := aSelector.
	extractionInterval := anInterval
initialization
combine: anInterval from: aSelector in: aClass
	class := self classObjectFor: aClass.
	selector := aSelector.
	selectedInterval := anInterval
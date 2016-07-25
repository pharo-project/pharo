initialization
split: anInterval from: aSelector in: aClass
	class := self classObjectFor: aClass.
	selector := aSelector.
	selectedInterval := anInterval
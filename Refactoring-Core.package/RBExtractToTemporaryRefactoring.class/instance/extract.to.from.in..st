initialize-release
extract: anInterval to: aString from: aSelector in: aClass 
	class := self classObjectFor: aClass.
	selector := aSelector.
	sourceInterval := anInterval.
	newVariableName := aString
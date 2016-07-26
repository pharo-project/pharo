initialize-release
sendersOf: aSelector in: aClass 
	class := self classObjectFor: aClass.
	selector := aSelector.
	numberReplaced := numberNotReplaced := 0
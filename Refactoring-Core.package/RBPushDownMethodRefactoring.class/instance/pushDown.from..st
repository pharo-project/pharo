initialize-release
pushDown: selectorCollection from: aClass 
	class := self classObjectFor: aClass.
	selectors := selectorCollection
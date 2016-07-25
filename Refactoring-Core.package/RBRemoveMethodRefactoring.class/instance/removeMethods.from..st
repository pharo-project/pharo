initialize-release
removeMethods: selectorCollection from: aClass 
	class := self classObjectFor: aClass.
	selectors := selectorCollection
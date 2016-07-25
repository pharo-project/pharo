initialize-release
pullUp: selectorCollection from: aClass 
	class := self classObjectFor: aClass.
	selectors := selectorCollection.
	removeDuplicates := false
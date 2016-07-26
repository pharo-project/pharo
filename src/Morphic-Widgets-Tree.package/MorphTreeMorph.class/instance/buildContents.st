updating
buildContents
	nodeList := nil.
	scroller removeAllMorphs.
	(self nodeList isNil or: [self nodeList isEmpty]) 
		ifTrue: [
			nodeList := nil.	
			^ self emptySelection].
	self addSubmorphsFromNodeList.

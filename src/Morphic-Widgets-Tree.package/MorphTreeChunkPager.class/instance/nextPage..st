navigation
nextPage: newLast
	| addedNodeList |
	newLast > self lastIndex
		ifFalse: [ ^ self ].
	self lastIndex < self nodeList size
		ifTrue: [ 
			addedNodeList := self nodeList copyFrom: 1 to: newLast.
			self lastIndex: newLast.
			treeMorph scroller removeAllMorphs.
			treeMorph addSubmorphsFromNodeList: addedNodeList previouslyExpanded: treeMorph currentlyExpanded ]
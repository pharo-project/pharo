updating
updateContentsWithPreviouslyExpanded: aNodeList
	"Make sure we notice the removal of the contained nodes."

	nodeList := nil.
	self noteRemovalOfAll: self allNodeMorphs.	"<-- Changed"
	(self nodeList isNil or: [ self nodeList isEmpty ])
		ifTrue: [ 
			nodeList := nil.
			^ self emptySelection ].
	self addSubmorphsFromNodeList: self currentNodelist previouslyExpanded: aNodeList.
	self listManager updateLastClickedMorph
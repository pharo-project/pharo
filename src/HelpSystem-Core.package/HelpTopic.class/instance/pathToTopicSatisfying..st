accessing
pathToTopicSatisfying: aBlock
	"I return the complete path to the first topic satifying the given block.
	The path is a collection starting with the root node and ending with the 
	node satifying the given block. If no node is found I return an empty collection."
	
	(aBlock value: self) ifTrue: [ ^ {self} ].
	
	self subtopics do: [ :aHelpTopic |
		(aHelpTopic pathToTopicSatisfying: aBlock)
			ifNotEmpty: [ :aPath |
				^ { self }, aPath ] ].
	^ #()
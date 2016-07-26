testing
hasMoreSentences
	^ selectedNode parent isNotNil and: [ selectedNode parent isMethod not ]
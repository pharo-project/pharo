accessing
findNode: aModel
	^ self nodes detect: [:aNode | aNode model = aModel ]
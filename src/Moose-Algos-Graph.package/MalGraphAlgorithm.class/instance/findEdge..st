accessing
findEdge: aModel
	^ self edges detect: [:edge | edge model = aModel]
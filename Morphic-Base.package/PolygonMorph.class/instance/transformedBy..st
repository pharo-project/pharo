geometry
transformedBy: aTransform
	self setVertices: (self vertices collect:[:v| aTransform localPointToGlobal: v])
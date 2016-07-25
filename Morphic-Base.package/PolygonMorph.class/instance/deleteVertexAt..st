editing
deleteVertexAt: anIndex
	"This acts as a backstop for MixedCurveMorph."
	self setVertices: (vertices
						copyReplaceFrom: anIndex
						to: anIndex
						with: #()).
						
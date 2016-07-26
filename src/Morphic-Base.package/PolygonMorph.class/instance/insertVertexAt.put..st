editing
insertVertexAt: anIndex put: aValue
	"This serves as a hook and a backstop for MixedCurveMorph."
	self setVertices: (vertices copyReplaceFrom: anIndex + 1 to: anIndex with: (Array with: aValue)).
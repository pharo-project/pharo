dashes
removeVertex: aVert
	"Make sure that I am not left with less than two vertices"
	| newVertices |
	vertices size < 2 ifTrue: [ ^self ].
	newVertices := vertices copyWithout: aVert.
	newVertices size caseOf: {
		[1] -> [ newVertices := { newVertices first . newVertices first } ].
		[0] -> [ newVertices := { aVert . aVert } ]
	} otherwise: [].
	self setVertices: newVertices 

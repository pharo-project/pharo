geometry
reduceVertices
	"Reduces the vertices size, when 3 vertices are on the same line with a little epsilon."
	| dup |
	[ (dup := self nextDuplicateVertexIndex) > 0 ] whileTrue: [
		self setVertices: (vertices copyWithoutIndex: dup)
	].
	^vertices size.
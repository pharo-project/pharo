editing
dropVertex: ix event: evt fromHandle: handle
	"Leave vertex in new position. If dropped ontop another vertex delete this one. Check for too few vertices before deleting. The alternative is not pretty"
	
	| p |
	p := vertices at: ix.
	(vertices size >= 2
			and: ["check for too few vertices before deleting. The alternative 
				is not pretty -wiz"
				((vertices atWrap: ix - 1)
						dist: p)
						< 3
					or: [((vertices atWrap: ix + 1)
							dist: p)
							< 3]])
		ifTrue: ["Drag a vertex onto its neighbor means delete"
				self deleteVertexAt: ix .].
	evt shiftPressed
		ifTrue: [self removeHandles]
		ifFalse: [self addHandles
			"remove then add to recreate"]
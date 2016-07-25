editing
addHandles
	"Put moving handles at the vertices. Put adding handles at
	edge midpoints.
	Moving over adjacent vertex and dropping will delete a
	vertex. "
	| tri |
	self removeHandles.
	handles := OrderedCollection new.
	tri := Array
				with: 0 @ -4
				with: 4 @ 3
				with: -3 @ 3.
	vertices
		withIndexDo: [:vertPt :vertIndex | | handle newVert | 
			handle := EllipseMorph
						newBounds: (Rectangle center: vertPt extent: 8 @ 8)
						color: (self handleColorAt: vertIndex) .
			handle
				on: #mouseMove
				send: #dragVertex:event:fromHandle:
				to: self
				withValue: vertIndex.
			handle
				on: #mouseUp
				send: #dropVertex:event:fromHandle:
				to: self
				withValue: vertIndex.
				handle
				on: #click
				send: #clickVertex:event:fromHandle:
				to: self
				withValue: vertIndex.
			self addMorph: handle.
			handles addLast: handle.
			(closed
					or: [1 = vertices size
						"Give a small polygon a chance to grow. 
						-wiz"
					or: [vertIndex < vertices size]])
				ifTrue: [newVert := PolygonMorph
								vertices: (tri
										collect: [:p | p + (vertPt
													+ (vertices atWrap: vertIndex + 1) // 2)])
								color: Color green
								borderWidth: 1
								borderColor: Color black.
					newVert
						on: #mouseDown
						send: #newVertex:event:fromHandle:
						to: self
						withValue: vertIndex.
					self addMorph: newVert.
					handles addLast: newVert]].
	self isCurvy
		ifTrue: [self updateHandles; layoutChanged].
	self changed
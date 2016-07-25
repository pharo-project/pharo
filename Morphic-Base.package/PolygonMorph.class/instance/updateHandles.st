editing
updateHandles
	| newVert |
	self isCurvy
		ifTrue: [handles first center: vertices first.
			handles last center: vertices last.
			self midVertices
				withIndexDo: [:midPt :vertIndex | (closed
							or: [vertIndex < vertices size])
						ifTrue: [newVert := handles atWrap: vertIndex * 2.
							newVert position: midPt - (newVert extent // 2)]]]
		ifFalse: [vertices
				withIndexDo: [:vertPt :vertIndex | | oldVert | 
					oldVert := handles at: vertIndex * 2 - 1.
					oldVert position: vertPt - (oldVert extent // 2).
					(closed
							or: [vertIndex < vertices size])
						ifTrue: [newVert := handles at: vertIndex * 2.
							newVert position: vertPt
									+ (vertices atWrap: vertIndex + 1) - newVert extent // 2 + (1 @ -1)]]]
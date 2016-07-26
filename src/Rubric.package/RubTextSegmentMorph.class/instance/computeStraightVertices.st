private
computeStraightVertices
	| firstCB lastCB firstLineIndex lastLineIndex firstLine lastLine verts secondLine thirdLine |
	firstCB := self characterBlockForIndex: firstIndex.
	lastCB := self characterBlockForIndex: lastIndex.
	firstLineIndex := self lineIndexOfCharacterIndex: firstIndex.
	lastLineIndex := self lineIndexOfCharacterIndex: lastIndex.
	firstLine := self lines at: firstLineIndex.
	lastLine := self lines at: lastLineIndex.
	verts := OrderedCollection new.
	firstLine = lastLine
		ifTrue: [ 
			verts add: firstCB bottomLeft.
			verts add: firstCB topLeft.
			firstIndex ~= lastIndex
				ifTrue: [ 
					verts add: lastCB topLeft.
					verts add: lastCB bottomLeft.
					verts add: firstCB bottomLeft ] ]
		ifFalse: [ 
			secondLine := self lines at: firstLineIndex + 1.
			thirdLine := self lines at: lastLineIndex - 1.
			verts add: firstCB bottomLeft - (1 @ 0).
			verts add: firstCB topLeft - (1 @ 0).
			verts add: (self textArea right - self margins right) @ firstLine top.
			verts add: (self textArea right - self margins right) @ thirdLine bottom.
			verts add: lastCB topLeft.
			verts add: lastCB bottomLeft.
			verts add: lastLine bottomLeft - (1 @ 0).
			verts add: secondLine topLeft - (1 @ 0).
			verts add: firstCB bottomLeft - (1 @ 0) ].
	self setVertices: verts
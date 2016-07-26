private
computeSmoothVertices
	| firstCB lastCB firstLineIndex lastLineIndex firstLine lastLine verts |
	firstLineIndex := self lineIndexOfCharacterIndex: firstIndex.
	lastLineIndex := self lineIndexOfCharacterIndex: lastIndex.
	firstLine := self lines at: firstLineIndex.
	lastLine := self lines at: lastLineIndex.
	verts := OrderedCollection new.
	firstLine = lastLine
		ifTrue: [ 
			firstCB := self textArea paragraph firstNonBlankCharacterBlockInLine: firstLine.
			verts add: firstCB bottomLeft.
			verts add: firstCB topLeft.
			firstIndex ~= lastIndex
				ifTrue: [ 
					lastCB := self textArea paragraph lastNonBlankCharacterBlockInLine: firstLine.
					verts add: lastCB topLeft.
					verts add: lastCB bottomLeft.
					verts add: firstCB bottomLeft ] ]
		ifFalse: [ 
			| currLine currIdx cb |
			currIdx := firstLineIndex.
			currLine := firstLine.
			[ 
			cb := self textArea paragraph lastNonBlankCharacterBlockInLine: currLine.
			verts add: cb topRight.
			verts add: cb bottomRight.
			currIdx := currIdx + 1.
			currLine := self lines at: currIdx ] doWhileTrue: [ currLine ~~ lastLine ].
			cb := self textArea paragraph lastNonBlankCharacterBlockInLine: currLine.
			verts add: cb topRight.
			verts add: cb bottomRight.
			[ 
			cb := self textArea paragraph firstNonBlankCharacterBlockInLine: currLine.
			verts add: cb bottomLeft - (1 @ 0).
			verts add: cb topLeft - (1 @ 0).
			currIdx := currIdx - 1.
			currLine := self lines at: currIdx ] doWhileTrue: [ currLine ~~ firstLine ].
			cb := self textArea paragraph firstNonBlankCharacterBlockInLine: currLine.
			verts add: cb bottomLeft - (1 @ 0).
			verts add: cb topLeft - (1 @ 0) ].
	self setVertices: verts
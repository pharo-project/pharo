querying
verticesFrom: firstIndex to: lastIndex
	| firstCB lastCB firstLineIndex lastLineIndex firstLine lastLine vertices secondLine thirdLine |
	firstCB := self characterBlockForIndex: firstIndex.
	lastCB := self characterBlockForIndex: lastIndex.
	firstLineIndex := self lineIndexOfCharacterIndex: firstIndex.
	lastLineIndex := self lineIndexOfCharacterIndex: lastIndex.
	firstLine := self lines at: firstLineIndex.
	lastLine := self lines at: lastLineIndex.
	vertices := OrderedCollection new.
	firstLine = lastLine
		ifTrue: [ 
			vertices add: firstCB bottomLeft.
			vertices add: firstCB topLeft.
			firstIndex ~= lastIndex
				ifTrue: [ 
					vertices add: lastCB topLeft.
					vertices add: lastCB bottomLeft.
					vertices add: firstCB bottomLeft ] ]
		ifFalse: [ 
			secondLine := self lines at: firstLineIndex + 1.
			thirdLine := self lines at: lastLineIndex - 1.
			vertices add: firstCB bottomLeft - (1@0).
			vertices add: firstCB topLeft - (1@0).
			vertices add: (self textArea right - self margins right) @ firstLine top.
			vertices add: (self textArea right - self margins right) @ thirdLine bottom.
			vertices add: lastCB topLeft.
			vertices add: lastCB bottomLeft.
			vertices add: lastLine bottomLeft - (1@0).
			vertices add: secondLine topLeft - (1@0).
			vertices add: firstCB bottomLeft - (1@0)].
	^ vertices
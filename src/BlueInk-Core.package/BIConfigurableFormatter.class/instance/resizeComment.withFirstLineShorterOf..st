utility
resizeComment: aComment withFirstLineShorterOf: anIndex
	| cutComment firstLine currentCharPos |
	currentCharPos := 1.
	firstLine := (aComment contents withNoLineLongerThan: self maxLineLength - anIndex)
		lineNumber: 1.
	[ currentCharPos > firstLine size ]
		whileFalse:
			[ 
			codeStream nextPut: (firstLine at: currentCharPos).
			currentCharPos := currentCharPos + 1 ].
	currentCharPos > aComment contents size
		ifFalse:
			[ 
			self newLine.
			cutComment := aComment contents allButFirst: currentCharPos.
			self resizeStringDependingOnWindowSizeFor: cutComment ]
utility
resizeStringDependingOnWindowSizeFor: aComment
	| resizedComment |
	resizedComment := aComment contents withNoLineLongerThan: self maxLineLength.
	1 to: resizedComment size do: [ :i | 
		codeStream nextPut: (resizedComment at: i).
		(resizedComment at: i) = Character cr
			ifTrue: [ self indent timesRepeat: [ codeStream nextPutAll: self indentString ] ] ]
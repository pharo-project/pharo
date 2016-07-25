accessing
lineNumberCorrespondingToIndex: anIndex
	"Answer a lineNumber for the line at the given character position."
	|lineNumber|
	
	lineNumber := 0.
	
	self lineIndicesDo: [:start :endWithoutDelimiters :end |
		lineNumber := lineNumber + 1.
		anIndex <= end ifTrue: [^lineNumber]].
	^lineNumber
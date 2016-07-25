accessing
lineCorrespondingToIndex: anIndex
	"Answer a string containing the line at the given character position."

	self lineIndicesDo: [:start :endWithoutDelimiters :end |
		anIndex <= end ifTrue: [^self copyFrom: start to: endWithoutDelimiters]].
	^''
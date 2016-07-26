source code management
linesOfCode
	"An approximate measure of lines of code.
	Includes comments, but excludes empty lines."
	| lines |
	lines := 0.
	self sourceCode lineIndicesDo: [:start :endWithoutDelimiters :end |
		endWithoutDelimiters > start ifTrue: [lines := lines+1]].
	^lines
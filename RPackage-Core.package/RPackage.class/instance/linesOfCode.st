statistics
linesOfCode
	"An approximate measure of lines of code.
	Includes comments, but excludes blank lines."
	^self methods inject: 0 into: [:sum :each | sum + each linesOfCode]
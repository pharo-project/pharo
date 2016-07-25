accessing
startOfLastStatement: anInteger
	"Note the source index of the start of the last full statement.  The
	 last full statement is the value answered by a block and hence the
	 expression the debugger should display as the value of the block."
	startOfLastStatement := anInteger
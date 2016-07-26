regression test
logProblem: reason for: aFile
	| errFile |
	errFile := FileStream fileNamed:'problems.log'.
	errFile position: errFile size.
	errFile cr; nextPutAll: aFile name;
			cr; nextPutAll: reason.
	errFile close.
	self trace:' failed (', reason,')'.
	aFile close.
	^false
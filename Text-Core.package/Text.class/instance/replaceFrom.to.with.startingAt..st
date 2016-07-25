converting
replaceFrom: start to: stop with: replacement startingAt: repStart 
 	"This destructively replaces elements from start to stop in the receiver starting at index, repStart, in replacementCollection. Do it to both the string and the runs."
 
 	| rep newRepRuns |
 	rep := replacement asText.	"might be a string"
 	string replaceFrom: start to: stop with: rep string startingAt: repStart.
 	newRepRuns := rep runs copyFrom: repStart to: repStart + stop - start.
	runs := runs copyReplaceFrom: start to: stop with: newRepRuns
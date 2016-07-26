private
newlineIndent
	prettyPrint ifTrue: [ 
		writeStream nextPutAll: newLine.
		level timesRepeat: [ writeStream tab ] ]
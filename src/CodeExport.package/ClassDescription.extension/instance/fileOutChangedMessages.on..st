*CodeExport
fileOutChangedMessages: aSet on: aFileStream
	"File a description of the messages of this class that have been 
	changed (i.e., are entered into the argument, aSet) onto aFileStream."

	| org |
	(org := self organization) categories
		do: [ :cat | 
			| sels |
			sels := (org listAtCategoryNamed: cat) select: [ :sel | aSet includes: sel ].
			sels do: [ :sel | self printMethodChunk: sel on: aFileStream ] ]
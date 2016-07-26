filein/out
preambleTemplate
	"Answer a string that will form the default contents for a change set's preamble.
	Just a first stab at what the content should be."

	^ String streamContents: [:strm |
		strm nextPutAll: '"Change Set:'.  "NOTE: fileIn recognizes preambles by this string."
		strm tab;tab; nextPutAll: self name.
		strm cr; nextPutAll: 'Date:'; tab; tab; tab; nextPutAll: Date today printString.
		strm cr; nextPutAll: 'Author:'; tab; tab; tab; nextPutAll:  Author fullName.
		strm cr; cr; nextPutAll: '<your descriptive text goes here>"']
"ChangeSet current preambleTemplate"
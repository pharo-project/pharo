actions
selectFileFromPattern
	"If there is a single file matching the pattern then select it.
	If none then try for a directory."

	| f matches subMatches |
	f := self files.
	matches := f select: [ :de | self fileNamePattern match: de name ].
	subMatches := matches reject: [ :de | de isDirectory ].
	subMatches size = 1
		ifTrue: [ ^ self selectedFileIndex: (f indexOf: subMatches first) ].
	subMatches := matches select: [ :de | de isDirectory ].
	subMatches size = 1
		ifTrue: [ ^ self selectedFileIndex: (f indexOf: subMatches first) ].
	self selectedFileIndex: 0
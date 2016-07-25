actions
selectedFileEntry
	"Answer the selected file."

	self selectedFileIndex = 0 ifTrue: [^nil].
	^self files at: self selectedFileIndex ifAbsent: [nil]
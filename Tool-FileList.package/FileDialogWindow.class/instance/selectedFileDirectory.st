actions
selectedFileDirectory
	"Answer the selected file directory in the tree part."

	^ (self selectedDirectory ifNil: [^nil]) item
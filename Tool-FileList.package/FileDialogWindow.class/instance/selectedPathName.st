actions
selectedPathName
	"Answer the name of the selected path."

	selectedDirectory := self selectedFileDirectory ifNil: [^nil].
	^ self selectedFileName 
		ifNil: [ selectedDirectory fullName ]
		ifNotNil: [ :filename | (selectedDirectory / filename) fullName ].
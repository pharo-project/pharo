actions
selectedFileName

	^ self selectedFileEntry ifNotNil: [ :dir | dir basename ]
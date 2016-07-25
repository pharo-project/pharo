accessing-system
imageName
	^ Smalltalk image imageName copyAfterLast: (Smalltalk globals at: #FileDirectory) slash first
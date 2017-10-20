initialization
registerEvents

	displayBlock whenChangedDo: [ self changed: #displayBlockChanged ].
	resizable whenChangedDo: [ self changed: #resizableChanged ].
	headerLabel whenChangedDo: [ self changed: #headerLabelChanged ].
	headerFont whenChangedDo: [ self changed: #headerFontChanged ].
	headerIcon whenChangedDo: [ self changed: #headerIconChanged ]
actions
selectionIsConflicted
	selection ifNil: [ ^ false ].
	^ selection isConflict
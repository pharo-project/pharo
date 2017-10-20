api
cancelled

	^ self window
		ifNil: [ false ]
		ifNotNil: [ :w | w cancelled ]
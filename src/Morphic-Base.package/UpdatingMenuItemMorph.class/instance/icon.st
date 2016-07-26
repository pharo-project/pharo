accessing
icon

	^ (target perform: iconSelector)
		ifNil: [ super icon ]
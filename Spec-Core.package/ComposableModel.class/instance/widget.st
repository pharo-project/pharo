accessing
widget

	^ spec
		ifNil: [ nil ]
		ifNotNil: [:s | s instance ]
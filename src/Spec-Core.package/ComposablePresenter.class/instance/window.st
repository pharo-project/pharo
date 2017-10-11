accessing
window

	^ window value
		ifNil: [
			owner
				ifNil: [ nil ]
				ifNotNil: [:o | o window ]]
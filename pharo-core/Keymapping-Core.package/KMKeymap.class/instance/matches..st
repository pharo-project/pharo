matching
matches: anEventBuffer

	^ anEventBuffer
		ifEmpty: [ false ]
		ifNotEmpty: [ self shortcut matches: anEventBuffer ]
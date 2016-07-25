private
monitor
	monitor isNil
		ifTrue: [monitor := Monitor new].
	^ monitor
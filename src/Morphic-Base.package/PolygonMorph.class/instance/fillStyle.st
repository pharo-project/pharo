accessing
fillStyle

	^ self isOpen
		ifTrue: [ self borderColor  "easy access to line color from halo"]
		ifFalse: [ super fillStyle ]
accessing
fillStyle: newColor

	^ self isOpen
		ifTrue: [ self borderColor: newColor asColor "easy access to line color from halo"]
		ifFalse: [ super fillStyle: newColor ]
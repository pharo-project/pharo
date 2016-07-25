protocol
at: anElement

	^ storage detect: [:each | each key = anElement]
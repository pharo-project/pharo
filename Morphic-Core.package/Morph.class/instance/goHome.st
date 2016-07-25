geometry
goHome
	| box fb |
	owner ifNil: [^ self].
	self visible ifFalse: [^ self].

	box := owner visibleClearArea.
	fb := self fullBounds.

	fb left < box left
		ifTrue: [self left: box left - fb left + self left].
	fb right > box right
		ifTrue: [self right: box right - fb right + self right].

	fb top < box top
		ifTrue: [self top: box top - fb top + self top].
	fb bottom > box bottom
		ifTrue: [self bottom: box bottom - fb bottom + self bottom].

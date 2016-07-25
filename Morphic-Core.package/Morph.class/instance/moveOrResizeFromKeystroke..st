event handling
moveOrResizeFromKeystroke: anEvent
	"move or resize the receiver based on a keystroke"

	| dir |
	anEvent keyValue = 28
		ifTrue: [ dir := -1 @ 0 ].
	anEvent keyValue = 29
		ifTrue: [ dir := 1 @ 0 ].
	anEvent keyValue = 30
		ifTrue: [ dir := 0 @ -1 ].
	anEvent keyValue = 31
		ifTrue: [ dir := 0 @ 1 ].
	dir notNil
		ifTrue: [ 
			anEvent controlKeyPressed
				ifTrue: [ dir := dir * 10 ].
			anEvent shiftPressed
				ifTrue: [ self extent: self extent + dir ]
				ifFalse: [ self position: self position + dir ] ]
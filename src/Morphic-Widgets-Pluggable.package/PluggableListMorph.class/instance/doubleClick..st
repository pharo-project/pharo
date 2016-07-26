events
doubleClick: event
	| index |
	doubleClickSelector ifNil: [^super doubleClick: event].
	index := self rowAtLocation: event position.
	index = 0 ifTrue: [^super doubleClick: event].
	"selectedMorph ifNil: [self setSelectedMorph: aMorph]."
	^ self model perform: doubleClickSelector
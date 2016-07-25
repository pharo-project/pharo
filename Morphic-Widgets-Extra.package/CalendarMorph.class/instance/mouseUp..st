event handling
mouseUp: event
	"Check for hotspot hits - handle them if they match.
	Otherwise, convert the event cursor to morph-local, and find the day under it.
	If there is nothing under the mouse when it goes up, nothing happens."

	| morphRelativeHitPoint |
	touchPoints keysAndValuesDo: [:eachBox :eachSelector |
		(eachBox containsPoint: event cursorPoint)
			ifTrue: [self perform: eachSelector]].
	morphRelativeHitPoint := event cursorPoint translateBy: bounds origin negated.
	days do: [:each |
		each highlighted: false.
		(each bounds containsPoint: morphRelativeHitPoint)
			ifTrue: [
				self date: each date.
				self announceDate ]].
	self changed.

accessing
milliseconds: currentTime since: lastTime
	"Answer the elapsed time since last recorded in milliseconds.
	Compensate for rollover."

	| delta |
	delta := currentTime - lastTime.
	^ delta < 0
		ifTrue: [SmallInteger maxVal + delta]
		ifFalse: [delta]

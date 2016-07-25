printing
intervalString
	"Treat the time as a difference.  Give it in hours and minutes with two digits of accuracy."

	| d |
	d := self asDuration.
	^ String streamContents: [ :s |
		d hours > 0 ifTrue: [s print: d hours; nextPutAll: ' hours'].
		d minutes > 0 ifTrue: [s space; print: d minutes; nextPutAll: ' minutes'].
		d seconds > 0 ifTrue: [s space; print: d seconds; nextPutAll: ' seconds'] ].


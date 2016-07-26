tablet
tabletPressure
	"Answer the current pressure of the first tablet pointing device (pen, puck, or eraser), a number between 0.0 (no pressure) and 1.0 (max pressure)"

	| params data |
	params := self primTabletGetParameters: 1.
	params ifNil: [^ self].
	data := self primTabletRead: 1.  "state of first/primary pen"
	^ (data at: 10) asFloat / ((params at: 10) - 1)

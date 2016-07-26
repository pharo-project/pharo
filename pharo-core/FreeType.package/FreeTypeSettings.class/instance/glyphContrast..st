accessing
glyphContrast: aContrastValue
	" value between 1 and 100.
	100 is highest contrast and maps to gamma 0.25
	1 is lowest contrast and maps to gamma 2.22"
	Cursor wait showWhile: [
		| v g |
		v := (((aContrastValue asNumber) min: 100) max: 1) asFloat.
		(v closeTo: 50.0) 
			ifTrue: [g := 1.0]
			ifFalse: [g := ((100 - v) + 50 / 100.0) raisedTo: 2].
		self setGamma: g].

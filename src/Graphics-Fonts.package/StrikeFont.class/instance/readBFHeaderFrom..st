file in/out
readBFHeaderFrom: f 
	name := self 
		restOfLine: 'Font name = '
		from: f.
	ascent := (self 
		restOfLine: 'Ascent = '
		from: f) asNumber.
	descent := (self 
		restOfLine: 'Descent = '
		from: f) asNumber.
	maxWidth := (self 
		restOfLine: 'Maximum width = '
		from: f) asNumber.
	pointSize := (self 
		restOfLine: 'Font size = '
		from: f) asNumber.
	name := (name copyWithout: Character space) , (pointSize < 10 
			ifTrue: [ '0' , pointSize printString ]
			ifFalse: [ pointSize printString ]).
	minAscii := 258.
	maxAscii := 0.
	superscript := (ascent - descent) // 3.
	subscript := (descent - ascent) // 3.
	emphasis := 0.
	type := 0	"ignored for now"
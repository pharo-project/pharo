private
debugDraw2
	| canvas last max t next |
	canvas := Display getCanvas.
	max := 100.
	last := nil.
	0 to: max do:[:i|
		t := i asFloat / max asFloat.
		next := self valueAt: t.
		last ifNotNil:[
			canvas line: last to: next rounded width: 2 color: Color blue.
		].
		last := next rounded.
	].
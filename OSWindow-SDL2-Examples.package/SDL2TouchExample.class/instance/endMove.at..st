manage move
endMove: touchId at: position
	(moveToDraw at: touchId) finish: position * self defaultExtent.
	[ 
	| d |
	d := Delay forMilliseconds: 3000.
	d wait.
	moveToDraw removeKey: touchId ifAbsent: [  ] ] fork
zoom animation
startZooming: zoom center: center
	| start end |

		
	start := Time millisecondClockValue.
	end := start + 250.

	self setState: #zooming value: {  center. transform copy. zoom. start. end}.

	self startStepping
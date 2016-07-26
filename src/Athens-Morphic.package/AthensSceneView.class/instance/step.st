zoom animation
step

	self inState: #zooming do: [ :zoomState |
		| now start end center targetZoom factor |
		now := Time millisecondClockValue.
		start := zoomState at: 4.
		end := zoomState at: 5.
		
		(now between: start and:end) ifFalse: [ now := end.
			self resetState: #zooming.
			self stopStepping ].
		
		transform := (zoomState at: 2) copy.
		center := zoomState at: 1.
		
		factor :=  (now-start)/(end - start ).
		targetZoom := zoomState at: 3.
		targetZoom := 1* (1-factor) + (targetZoom * factor).
		
		transform
			translateBy: center;
			scaleBy:targetZoom;
			translateBy: center negated.
		
	].
	self changed.

displaying-display
slideWithFirstFrom: startPoint to: stopPoint nSteps: nSteps delay: milliSecs 
	"Slide this object across the display over the given number of steps, 
	pausing for the given number of milliseconds after each step."
	"Note: Does display at the first point and at the last."
	| i p delta |
	i := 0.
	delta := stopPoint - startPoint / nSteps asFloat.
	p := startPoint - delta.
	^ self 
		follow: [(p := p + delta) truncated]
		while: 
			[(Delay forMilliseconds: milliSecs) wait.
			(i := i + 1) <= nSteps]
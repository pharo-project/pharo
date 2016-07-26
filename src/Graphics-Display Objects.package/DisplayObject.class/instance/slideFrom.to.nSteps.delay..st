displaying-display
slideFrom: startPoint to: stopPoint nSteps: nSteps delay: milliSecs
	"Slide this object across the display over the given number of steps, pausing for the given number of milliseconds after each step."
	"Note: Does not display at the first point, but does at the last."

	| i p delta |
	i := 0.
	p := startPoint.
	delta := (stopPoint - startPoint) / nSteps asFloat.
	^ self
		follow: [
			p := p + delta.
			p truncated]
		while: [
			(Delay forMilliseconds: milliSecs) wait.
			(i := i + 1) < nSteps]

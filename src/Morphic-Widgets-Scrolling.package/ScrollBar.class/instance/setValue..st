model access
setValue: newValue
	"Using roundTo: instead of truncateTo: ensures that scrollUp will scroll the same distance as scrollDown.
	Fix for >= 1.0 since, when close the roundTo may round down and not allow the value to reach
	the full range."
	
	newValue >= 1.0
		ifTrue: [^super setValue: 1.0].
	^ super setValue: (newValue roundTo: scrollDelta)
testing
wantsSteps
	super wantsSteps ifTrue: [^true].

	"For crawling ants effect of dashed line."
	^ self isAnimated
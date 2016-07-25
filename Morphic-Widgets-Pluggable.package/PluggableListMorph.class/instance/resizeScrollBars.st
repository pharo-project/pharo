scrolling
resizeScrollBars
	"Fixed to not use deferred message that incorrectly
	sets scroll deltas/interval."

	(self extent = self defaultExtent)
		ifFalse: [super resizeScrollBars]
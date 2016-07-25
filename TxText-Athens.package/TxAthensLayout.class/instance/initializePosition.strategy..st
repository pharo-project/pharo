initialize-release
initializePosition: pos strategy: layoutStrategy
	"initialize layout and scan a single line using given strategy"
	| line |
	
	lines := OrderedCollection new.
	strategy := layoutStrategy.

	line := strategy scanSingleVisualLineFor: self startingAt: pos.
	
	self setLines: { line }
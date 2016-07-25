*Polymorph-Widgets
borderStyle: aBorderStyle
	"Optimised when no change."
	
	self borderStyle = aBorderStyle ifTrue: [^self].
	super borderStyle: aBorderStyle.
	self setScrollDeltas
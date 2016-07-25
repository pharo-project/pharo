*Polymorph-Widgets
shouldDraw
	"Answer whether the resizer should be drawn."
	
	^super shouldDraw or: [self class showSplitterHandles]
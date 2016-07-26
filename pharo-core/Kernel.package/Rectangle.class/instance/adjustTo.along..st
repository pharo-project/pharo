rectangle functions
adjustTo: newRect along: side 
	"Return a copy adjusted to fit a neighbor that has changed size."
	side = #left ifTrue: [^ self withRight: newRect left].
	side = #right ifTrue: [^ self withLeft: newRect right].
	side = #top ifTrue: [^ self withBottom: newRect top].
	side = #bottom ifTrue: [^ self withTop: newRect bottom].
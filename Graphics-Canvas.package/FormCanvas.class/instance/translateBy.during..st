drawing-support
translateBy: delta during: aBlock
	"Set a translation only during the execution of aBlock."
	^aBlock value: (self copyOffset: delta)
accessing
roots
	"Answer the receiver's roots"
	^ scroller submorphs
		select: [:each | each indentLevel isZero]
updating
changed: aParameter
	"overriden to reinstall the cursor if it is the active cursor, in case the appearance has changed.  (Is this used anywhere?  Do cursors really change in place these days?)"
	self == CurrentCursor ifTrue: [self beCursor].
	super changed: aParameter
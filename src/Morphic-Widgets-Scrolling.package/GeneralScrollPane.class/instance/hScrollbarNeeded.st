scrollbars
hScrollbarNeeded
	"Return whether the horizontal scrollbar is needed."
	
	self hScrollbar showsAlways ifTrue: [^true].
	self hScrollbar showsNever ifTrue: [^false].
	^self scrollTarget width +
		(self scrollTarget height > self innerBounds height
			 ifTrue: [self scrollBarThickness] ifFalse: [0]) >
		self innerBounds width
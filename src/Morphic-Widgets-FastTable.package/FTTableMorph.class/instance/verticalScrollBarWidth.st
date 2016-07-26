private
verticalScrollBarWidth
	self isVerticalScrollBarVisible ifFalse: [  ^ 0 ].
	^ self scrollBarThickness
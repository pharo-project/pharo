private
white
	Display depth = 1 ifTrue: [^ Bitmap with: 0 "Works without color support"].
	^ Color white
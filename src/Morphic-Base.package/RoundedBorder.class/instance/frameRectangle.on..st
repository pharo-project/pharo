drawing
frameRectangle: aRectangle on: aCanvas
	"Draw the border. Radius is the x/y offset not width 'around the corner'."
	
	self cornerRadius = 0 ifTrue: [^self frameRectangle0: aRectangle on: aCanvas].
	self cornerRadius = 1 ifTrue: [^self frameRectangle1: aRectangle on: aCanvas].
	self cornerRadius = 2 ifTrue: [^self frameRectangle2: aRectangle on: aCanvas].
	self cornerRadius = 3 ifTrue: [^self frameRectangle3: aRectangle on: aCanvas].
	self cornerRadius = 4 ifTrue: [^self frameRectangle4: aRectangle on: aCanvas].
	self cornerRadius = 5 ifTrue: [^self frameRectangle5: aRectangle on: aCanvas].
	self cornerRadius = 6 ifTrue: [^self frameRectangle6: aRectangle on: aCanvas].
	self cornerRadius = 7 ifTrue: [^self frameRectangle7: aRectangle on: aCanvas].
	self cornerRadius = 8 ifTrue: [^self frameRectangle8: aRectangle on: aCanvas].
	^super frameRectangle: aRectangle on: aCanvas.
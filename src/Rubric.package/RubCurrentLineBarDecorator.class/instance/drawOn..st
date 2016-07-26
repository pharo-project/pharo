drawing
drawOn: aCanvas
 
	| line bnd |
	
	aCanvas isShadowDrawing ifTrue: [ ^self ].
	
	self selectionStart = self selectionStop ifFalse: [^self].
	
	line := self selectionStart textLine.
	bnd := (line topLeft - (self margins left @ 0)) corner: (line bottomRight + (self margins right @ 0)).
	aCanvas fillRectangle: bnd color: self selectionBarColor
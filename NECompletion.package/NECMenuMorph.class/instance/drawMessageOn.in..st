drawing
drawMessageOn: aCanvas in: rectangle 
	self hasMessage ifFalse: [ ^ self ].
	context model isEmpty ifFalse: [ 
		aCanvas 
			line: rectangle topLeft
			to: rectangle topRight
			color: Color gray ].
	self 
		drawModelMessageOn: aCanvas
		in: rectangle.
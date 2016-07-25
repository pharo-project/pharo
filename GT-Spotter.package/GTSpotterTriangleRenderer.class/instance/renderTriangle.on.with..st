rendering
renderTriangle: aBrick on: aCanvas with: aCollection

	self renderTriangle: aBrick on: aCanvas with: aCollection color: (aBrick currentColor ifNil: [ aBrick color ])
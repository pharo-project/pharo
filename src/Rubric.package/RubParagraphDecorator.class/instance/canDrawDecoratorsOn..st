drawing
canDrawDecoratorsOn: aCanvas

	^self drawingEnabled and: [ 
		self selectionStart isNotNil and: [ 
			self selectionStop isNotNil
		]
	]
	
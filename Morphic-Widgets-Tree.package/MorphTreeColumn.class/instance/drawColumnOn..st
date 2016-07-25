converting
drawColumnOn: aCanvas
	self isPotentialDropTarget 
		ifTrue: [aCanvas 
					frameAndFillRectangle: self dataBounds 
					fillColor: ((Color gray alpha: 0.1) alphaMixed: 0.9 with: (self color ifNotNil: [:c | c asColor] ifNil: [Color transparent]))
					borderWidth: 2 
					borderColor: Color gray]
		ifFalse: [self color 
						ifNotNil: [:c | 
							c isColor 
								ifTrue: [
									aCanvas 
										frameAndFillRectangle: self dataBounds 
										fillColor: self color
										borderWidth: 0 
										borderColor: Color transparent]
								ifFalse: [c origin: self dataBounds topLeft.
									c direction: 0@self dataBounds height.
									aCanvas fillRectangle: self dataBounds  basicFillStyle: c]]]
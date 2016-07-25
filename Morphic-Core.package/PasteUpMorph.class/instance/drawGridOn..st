drawing
drawGridOn: aCanvas

	(self griddingOn and: [ self gridVisible ])
		ifTrue: [ 
			aCanvas
				fillRectangle: self bounds
				fillStyle:
					(self
						gridFormOrigin: self gridOrigin
						grid: self gridModulus
						background: nil
						line: Color lightGray) ]
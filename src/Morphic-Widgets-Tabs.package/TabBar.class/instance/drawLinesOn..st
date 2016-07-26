drawing
drawLinesOn: aCanvas

	self selectedTab 
		ifNil: [
			aCanvas
				line: self bottomLeft + (0 @ -1)
				to: self bottomRight + (0 @ -1)
				width: 1
				color: self borderColor ]
		ifNotNil: [
			aCanvas
				line: self bottomLeft + (0 @ -1)
				to: self selectedTab bottomLeft
				width: 1
				color: self borderColor.
				
			aCanvas
				line: self selectedTab bottomLeft
				to: self selectedTab  bottomRight
				width: 1
				color: self selectedColor.
				
			aCanvas
				line: self selectedTab  bottomRight
				to: self bottomRight + (0 @ -1)
				width: 1
				color: self borderColor ]
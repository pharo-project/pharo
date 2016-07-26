bordering
border: aRectangle width: borderWidth rule: combinationRule fillColor: aHalfTone 
	"Paint a border whose rectangular area is defined by aRectangle. The 
	width of the border of each side is borderWidth. Uses aHalfTone for 
	drawing the border."

	self border: aRectangle
		widthRectangle: 
			(Rectangle
				left: borderWidth
				right: borderWidth
				top: borderWidth
				bottom: borderWidth)
		rule: combinationRule
		fillColor: aHalfTone
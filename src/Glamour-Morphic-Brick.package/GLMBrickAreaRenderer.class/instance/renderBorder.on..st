rendering
renderBorder: aBrick on: aCanvas

	aCanvas
		drawPolygon: aBrick area vertices
		color: Color transparent
		borderWidth: aBrick border widthTop
		borderColor: aBrick border colorTop
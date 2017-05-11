rendering
renderBackgroundColorOf: aBrick on: aCanvas
	| rectangle |

	rectangle := aBrick brickBounds expandBy: aBrick shadowWidth negated.
	self renderBackgroundColorOf: aBrick on: aCanvas in: rectangle
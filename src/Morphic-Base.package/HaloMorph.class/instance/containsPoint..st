geometry testing
containsPoint: aPoint 
	"This method is overridden so that, once up, the handles will stay up as long as the mouse is within the box that encloses all the handles even if it is not over any handle or over its owner."

	^ target ifNil: [super containsPoint: aPoint] ifNotNil: [false]
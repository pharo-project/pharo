tests extending
pointRectangleInGraphElement 
	| p |
	p := self packageClass named: #GraphElement.
	p addClassDefinition: Point.
	p addMethod: Point>>#x.
	p addMethod: Point>>#rotateBy:about:. 
	p addClassDefinition: Rectangle.
	p addMethod: self quadrangleClass >>#intersect:.
	^ p
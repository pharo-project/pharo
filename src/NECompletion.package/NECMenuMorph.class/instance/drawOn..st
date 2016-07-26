drawing
drawOn: aCanvas 
	| rectangle model |
	"draw background"
	super drawOn: aCanvas.
	
	rectangle := self bounds insetBy: 1.
	rectangle := rectangle bottom: rectangle top + self class itemHeight.
	rectangle := rectangle left: rectangle left + 20.
	model := context model.
	
	self extent: self extent.
	self firstVisible > 1 
		ifTrue: [ self drawTopScrollArrowOn: aCanvas ].
	self lastVisible ~= self itemsCount 
		ifTrue: [ self drawBottomScrollArrowOn: aCanvas ].
		
	model notEmpty ifTrue: [ 
		self firstVisible to: self lastVisible do: [ :index | 
			self drawLine: index on: aCanvas rectangle: rectangle.
			rectangle := self prepareRectForNextRow: rectangle ]].
		
	self 
		drawMessageOn: aCanvas
		in: rectangle
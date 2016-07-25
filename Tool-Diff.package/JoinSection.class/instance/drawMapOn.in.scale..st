as yet unclassified
drawMapOn: aCanvas in: rect scale: scale
	"Draw the join on the given canvas scaled into the given rectangle."
	
	self type = #match ifTrue: [^self].
	aCanvas
		frameAndFillRectangle: (rect left @ (((self dst range first max: 0) * scale) truncated + rect top)
						corner: (rect right @ ((self dst range last * scale) truncated + rect top)))
		fillColor: (self fillStyleFor: rect)
		borderWidth: 1
		borderColor: self borderColorToUse
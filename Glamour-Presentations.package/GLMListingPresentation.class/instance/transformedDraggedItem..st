accessing values
transformedDraggedItem: anItem
	^ self transformDraggedItemBlock 
		ifNotNil: [ :transform | transform glamourValue: (GLMMultiValue with: anItem with: self)]
		ifNil: [ anItem ]
*GT-Inspector
gtInspectorVariableNodesIn: aCollection
	"This is a helper method that adds to the given collection all nodes modeling named and 
	indexed attributes for the current object.
	Subclasses can override it to specialize what appears in the variables presentation."
	| indexableDisplayLimit top bottom topLimit bottomLimit |
	
	indexableDisplayLimit := 21.        
	top := 1.
	bottom := self basicSize.
	topLimit    := indexableDisplayLimit min: bottom.
	bottomLimit := indexableDisplayLimit max: (bottom - indexableDisplayLimit).

	top to: topLimit do: [ :index | 
		aCollection add: (GTInspectorIndexedNode hostObject: self index: index) ].
	
	bottomLimit + 1 to: bottom do: [ :index | "self haltIf: [ index = 99 ]."
		aCollection add: (GTInspectorIndexedNode hostObject: self index: index) ].
	
	aCollection
		addAll: ((self class allSlots collect: [ :slot | 
			GTInspectorSlotNode hostObject: self slot: slot ]))
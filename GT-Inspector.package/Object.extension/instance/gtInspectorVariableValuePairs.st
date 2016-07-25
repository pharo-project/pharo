*GT-Inspector
gtInspectorVariableValuePairs
	"This is a helper method that returns a collection of 
		variable_name -> value
	for the current object.
	Subclasses can override it to specialize what appears in the variables presentation"
	| bindings |
	bindings := OrderedCollection new.
					
	1 to: (self basicSize min: 21) do: [ :index | 
		bindings add: (index "asString""asTwoCharacterString" -> (self basicAt: index)) ].
	
	((self basicSize - 20) max: 22) to: (self basicSize) do: [ :index | "self haltIf: [ index = 99 ]."
		bindings add: (index "asString" -> (self basicAt: index)) ].
	
	bindings
		addAll: ((self class allSlots 
					collect: [ :slot | slot name -> (slot read: self) ]) sort asOrderedCollection).
	^ bindings
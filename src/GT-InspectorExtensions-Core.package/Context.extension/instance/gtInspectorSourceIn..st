*GT-InspectorExtensions-Core
gtInspectorSourceIn: composite
	<gtInspectorPresentationOrder: 30> 
	(self method gtInspectorSourceIn: composite)
		doItReceiver: [ self receiver ];
		doItContext: [ self ];
		initialize: [ :aPresentation |
			self isDead ifFalse: [
				"Try to highlight the code that is currently being executed.
				This will not work for the top context of the process."
				aPresentation selectionInterval: (self debuggerMap 
					rangeForPC: self pc
					contextIsActiveContext: false)] ]
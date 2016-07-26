building presentations
methodCodeIn: composite forContext: aContext

	(self methodCodeWidgetIn: composite forContext: aContext)
		initialize: [ :code | 
			code selectionInterval: (self session selectedCodeRangeForContext: self currentContext) ];
		with: [ :code | self installCodeActionsFor: code ]
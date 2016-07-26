building presentations
methodCodeIn: composite forContext: aContext

	(self methodCodeWidgetIn: composite forContext: aContext)
		with: [ :code | self installCodeActionsFor: code ].
	"self decompiledCodeWidgetIn: composite forContext: aContext"
building widgets
dynamicLayoutForActions: aCollection
	"Create a two column layout for the subwidgets."

	^ SpecLayout composed
		newColumn: [ :c | 
			c newRow: [ :row |
				aCollection do: [ :anAction | row add: anAction specId ] ] height: self class toolbarHeight.
	  		c newRow: [ :row | row add: self notifierPaneWidgetId] ]
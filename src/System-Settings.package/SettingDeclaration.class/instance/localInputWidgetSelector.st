user interface
localInputWidgetSelector
	| imSel |
	^ (self respondsTo: (imSel := ('inputWidgetFor' , self type) asSymbol)) ifTrue: [imSel]
events
explicitlyInnerPortNamed: aPortName

	aPortName = #entity ifFalse: [
		self panes first ports do: [:each | (self pane port: each name) value: each value] ]
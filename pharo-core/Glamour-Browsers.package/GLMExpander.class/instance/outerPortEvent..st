events
outerPortEvent: aPortEvent 
	 
	aPortEvent port name = #entity ifFalse: [^self]. 
	panes := nil. 
	self matches ifTrue: [
		"We only want to execute the transformation if the visibility condition matches"
		(self transformation glamourValue: aPortEvent port value) do: [:each |
			self addPaneFrom: (GLMSimplePort new value: each) ] ]
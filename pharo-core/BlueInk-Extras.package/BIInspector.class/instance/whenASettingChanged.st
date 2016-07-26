event handling
whenASettingChanged
	(self formattedCheckBox state and: [ self selectorAndClassAreCorrect ])
		ifTrue: [ self formatSourceCode ] 
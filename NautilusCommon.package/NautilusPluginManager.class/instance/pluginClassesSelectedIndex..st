lists behavior
pluginClassesSelectedIndex: anInteger

	pluginClassesSelectedIndex := anInteger.
	self changed: #pluginClassesSelectedIndex.
	self textModel setText: (anInteger = 0 ifTrue: ['']
													 ifFalse: [ self getText ])
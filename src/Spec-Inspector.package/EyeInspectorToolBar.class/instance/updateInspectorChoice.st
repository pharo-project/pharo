event-handling
updateInspectorChoice
	self inspectorTypes
		ifNotNil: [ :variants | 
			self inspectorChoice listItems = variants
				ifTrue: [  ^ self ].
			self inspectorChoice
				items: variants;
				displayBlock: [ :label | label ]]
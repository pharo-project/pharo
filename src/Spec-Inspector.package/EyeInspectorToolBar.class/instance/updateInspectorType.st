event-handling
updateInspectorType
	
	
	self inspectorType
		ifNotNil: [ :inspectorClass | 
			self inspectorChoice selectedItem = inspectorClass
				ifTrue: [  ^ self ].
			self inspectorChoice setSelectedItem: inspectorClass ].
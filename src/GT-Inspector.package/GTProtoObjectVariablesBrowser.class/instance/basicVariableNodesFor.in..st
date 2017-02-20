private building
basicVariableNodesFor: anObject in: aCollection

	aCollection addAll: ((anObject class allInstVarNames 
			collectWithIndex: [ :ivName :index | 
				GTInspectorProtoObjectNode hostObject: anObject index: index slotName: ivName]) 
					sort: [:aNode :anotherNode | aNode label < anotherNode label ])
			
*GT-Debugger
gtDebuggerRetrieveVariableValuePairs
	|fieldList|

	fieldList := SortedCollection 
						sortBlock: [:variablePair1 :variablePair2 | 
							variablePair1 key < variablePair2 key].
	fieldList
		addAll: ((self receiver class allSlots collect: [ :slot | 
			GTInspectorSlotNode hostObject: self receiver slot: slot ])).
		
	fieldList addAll: self gtInspectorTempNodes.
	fieldList addAll: (self gtDebuggerDoItBindings collect: [:aBinding |
		(GTInspectorDynamicNode hostObject: self label: aBinding key value: aBinding value)
			variableTag: 'temp';
			yourself ]).
	
	^ fieldList asOrderedCollection 
		addFirst: (GTInspectorSelfNode hostObject: self receiver);
		addLast: (GTInspectorDynamicNode hostObject: self label: 'thisContext' value: self);
		addLast: (GTInspectorDynamicNode hostObject: self label: 'stack top' value: ( self stackPtr > 0 ifTrue: [ self top]));
		yourself

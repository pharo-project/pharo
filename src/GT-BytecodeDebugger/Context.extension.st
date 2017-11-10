Extension { #name : #Context }

{ #category : #'*GT-BytecodeDebugger' }
Context >> gtBytecodeDebuggerRetrieveContextValues [
	|fieldList displayedTemp slotsSize tempNames |

	fieldList := OrderedCollection new.
	displayedTemp := OrderedCollection new.
	tempNames := self tempNames.
	
	self stackPtr to: 1 by: -1 do: [ :tempIndex ||tempNode|
		fieldList add:
			(tempIndex <= tempNames size
				ifTrue: [ 
					displayedTemp add: (tempNames at: tempIndex).
					tempNode := GTInspectorContextNamedTempNode 
						hostObject: self 
						tempName: (tempNames at: tempIndex) 
						tempIndex: tempIndex.
					GTInspectorWrapperNode 
						hostObject: tempNode
						label: (tempIndex - 1) asString, ' [', (tempNames at: tempIndex), ']'  ]
				ifFalse: [
					 tempNode := GTInspectorContextIndexedTempNode hostObject: self tempIndex: tempIndex.
					 GTInspectorWrapperNode 
					 	hostObject: tempNode
						label: (tempIndex - 1) asString ]) ].
	
	(tempNames copy asOrderedCollection removeAll: displayedTemp; yourself) ifNotEmpty: [ :uninitializedTemps |
		uninitializedTemps do: [ :each |
			GTInspectorDynamicNode hostObject: self label: ('- [', each, ']') value: nil ] ].
	
	fieldList addAll: (self gtDebuggerDoItBindings collect: [:aBinding |
		(GTInspectorDynamicNode hostObject: self label: aBinding key value: aBinding value)
			variableTag: 'temp';
			yourself ]).
	
	slotsSize := self receiver class allSlots size.
	fieldList 
		addAll: ((self receiver class allSlots reverse) collect: [:slot ||slotNode|
			slotNode :=  GTInspectorSlotNode hostObject: self receiver slot: slot.
			GTInspectorWrapperNode 
				hostObject: slotNode
				label: ('rcv', (slotsSize := slotsSize - 1) asString, ' [', slot name , ']')  ]).
	
	^ fieldList asOrderedCollection 
		addFirst: (GTInspectorSelfNode hostObject: self receiver);
		addLast: (GTInspectorDynamicNode hostObject: self label: 'thisContext' value: self);
		yourself
]

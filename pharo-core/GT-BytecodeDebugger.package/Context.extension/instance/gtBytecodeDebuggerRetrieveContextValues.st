*GT-BytecodeDebugger
gtBytecodeDebuggerRetrieveContextValues
	|fieldList displayedTemp context slotsSize|

	fieldList := OrderedCollection new.
	context := self.
	context == nil ifTrue: [^('_thisContext' -> nil) asOrderedCollection].
	fieldList
		add: ('_self' ->  context receiver);
		add: ('_thisContext' ->  context).
	
	displayedTemp := OrderedCollection new.
	context stackPtr to: 1 by: -1 do: [ :index ||key|
		key := (index-1) asString.
		(index <= context tempNames size) ifTrue: [ 
			key := key, ' [', (context tempNames at: index) asString, ']'.
			displayedTemp add: (context tempNames at: index) ].
		fieldList add: (key -> (context at: index)) ].
	(context tempNames copy asOrderedCollection removeAll: displayedTemp; yourself) ifNotEmpty: [ :uninitializedTemps |
		uninitializedTemps do: [ :each |
			fieldList add: ('- [', each, ']') -> nil ] ].
	slotsSize := context receiver class allSlots size.
	fieldList 
		addAll: ((context receiver class allSlots reverse) collect: [:slot ||name| 
			name := slot name.
			('rcv', (slotsSize := slotsSize - 1) asString, ' [', name , ']') -> (slot read: context receiver) ]).
	
	^ fieldList
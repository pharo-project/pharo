*GT-Debugger
gtDebuggerRetrieveVariableValuePairs
	|fieldList context|

	fieldList := SortedCollection 
						sortBlock: [:variablePair1 :variablePair2 | 
							variablePair1 key < variablePair2 key].
	context := self.
	fieldList 
		addAll: (context receiver class allSlots collect: [:slot ||value| 
				value := [(slot read: context receiver)] 
					on: Error 
					do: [ value := (Text string: 'error obtaining field value' attribute: TextColor red) asString ].
			slot name -> value ] ).
	
	
	context isNil ifTrue: [
		^ fieldList asOrderedCollection 
			addLast: ('thisContext' -> context);
			yourself].
		
	(context tempNames notEmpty) ifTrue: [
		fieldList addAll: ((1 to: context tempNames size) collect: [ :index || key value |
			key := context tempNames at: index.
			value := [context debuggerMap namedTempAt: index in: context] 
				on: Error 
				do: [value := (Text string: 'error obtaining field value' attribute: TextColor red) asString ].
			key -> value ] ) ].
	
	^ fieldList asOrderedCollection 
		addFirst: ('self' ->  context receiver);
		addLast: ('thisContext' -> context);
		addLast: ('stack top' -> ( context stackPtr > 0 ifTrue: [ context top]));
		yourself

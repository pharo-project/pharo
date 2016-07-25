*gt-debugger
gtDebuggerRetrieveVariableValuePairs
	| fieldList |
	fieldList := OrderedCollection new.
	fieldList
		add: '_self' -> context receiver;
		addAll: (context receiver class allInstVarNames collect: [ :each | each -> (context receiver instVarNamed: each) ]).
	fieldList add: '_thisContext' -> context.
	context == nil
		ifTrue: [ ^ fieldList ].
	fieldList
		add:
			'_stack top'
				->
					(context stackPtr > 0
						ifTrue: [ context top ]).
	context tempNames notEmpty
		ifTrue: [ 
			fieldList
				addAll:
					((1 to: context tempNames size)
						collect: [ :index | 
							| key value |
							key := context tempNames at: index.
							value := [ context debuggerMap namedTempAt: index in: context ]
								on: Error
								do: [ value := (Text string: 'error obtaining field value' attribute: TextColor red) asString ].
							key -> value ]) ].
	^ fieldList
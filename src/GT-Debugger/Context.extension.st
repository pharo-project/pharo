Extension { #name : #Context }

{ #category : #'*GT-Debugger' }
Context >> findContextMatchingPredicate: aPredicate [
	^ self findContextSuchThat: [ :ctx |
		aPredicate matches: ctx ]
]

{ #category : #'*GT-Debugger' }
Context >> gtConstructDebuggerInspectorIn: composite for: aDebugger [
	
	^ composite custom: ((GTDebuggerInspector new  
		debugger: aDebugger;
		notifyPaneRemoved: true;
		showFirst: [ :a | a custom: (aDebugger class variablesBrowserClass new 
			debugger: aDebugger;
			startOn: self;
			restoreSettings;
			yourself).];
		yourself) 
			startOn: self)
]

{ #category : #'*GT-Debugger' }
Context >> gtDebuggerDoItBindings [
	^ (self method isDoIt and: [ self receiver isNil ]) 
		ifTrue: [ self method literals select: [ :each | 
			each class = WorkspaceVariable ] ]
		ifFalse: [ #() ]
]

{ #category : #'*GT-Debugger' }
Context >> gtDebuggerRetrieveVariableValuePairs [
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

]

{ #category : #'*GT-Debugger' }
Context >> reservedNames [

	^ #( 'self' 'thisContext' 'stack top')
]

Extension { #name : #ProtoObject }

{ #category : #'*GT-Debugger-UI' }
ProtoObject >> gtDebuggerEvaluatorIn: composite [
	<gtDebuggerPresentationOrder: 10>
	
	(GTProtoObjectVariablesBrowser new evaluatorIn: composite)
		title: 'Evaluator'
]

{ #category : #'*GT-Debugger-UI' }
ProtoObject >> gtDebuggerPresentationsIn: composite inContext: aGTInspector [
	| pragmas |
	pragmas := (Pragma 
				allNamed: #gtDebuggerPresentationOrder:
				from: self class 
				to: ProtoObject) asOrderedCollection.
	pragmas addAll:((Pragma 
				allNamed: aGTInspector class extensionsPragma 
				from: self class 
				to: ProtoObject)
					reject: [:pragma| pragma method selector = #gtInspectorRawIn: ]).
	pragmas := pragmas sorted: [: x :y | (x argumentAt: 1) < (y argumentAt: 1)].
	self gtInspectorPresentationsFromPragmas: pragmas In: composite inContext: aGTInspector

]

{ #category : #'*GT-Debugger-UI' }
ProtoObject >> gtDebuggerRawIn: composite [
	<gtDebuggerPresentationOrder: 15>
	
	(GTProtoObjectVariablesBrowser new variablesIn: composite)
		title: 'Raw';
		send: #value
]

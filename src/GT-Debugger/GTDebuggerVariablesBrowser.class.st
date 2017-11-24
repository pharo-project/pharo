"
I am a special browser that is used in the first pane of the inspector that is embedded in the debugger. Its main intention is to provide a unified list of variables that come from:
- the instance variables of the receiver
- the local variables from the context
"
Class {
	#name : #GTDebuggerVariablesBrowser,
	#superclass : #GLMCompositePresentation,
	#instVars : [
		'debugger',
		'cachedVariables'
	],
	#category : #'GT-Debugger-UI'
}

{ #category : #'private building' }
GTDebuggerVariablesBrowser >> buildVariablesFromContext: aContext [ 
	^ aContext gtDebuggerRetrieveVariableValuePairs
]

{ #category : #building }
GTDebuggerVariablesBrowser >> compose [  
	| tabulator |
	tabulator := self tabulator.
	tabulator row: #main.
	tabulator transmit to: #main; andShow: [ :aComposite | 
		aComposite tabulator with: [:browser | 
			browser title: 'Variables'.
			browser row: #variables.
			browser transmit 
				to: #variables port: #entity; 
				andShow: [:a | 
					self variablesIn: a ].
			browser transmit 
				from: #variables port: #entity; 
				to: #variables port: #selection; 
				when: [ :aContext | self debugger selectedVariableName notNil and: [aContext notNil] ];
				transformed: [ :aContext | self selectedVariableForContext: aContext ].
			browser transmit from: #variables; toOutsidePort: #selection; transformed: [ :aNode | aNode value ] ].
		aComposite tabulator with: [:browser | 
			browser title: 'Evaluator'.
			browser row: #evaluator.
			browser transmit to: #evaluator; andShow: [:a | 
				self contextEvaluatorIn: a. ].
			browser transmit from: #evaluator; toOutsidePort: #selection ].
		
		aComposite onChangeOfPort: #activePresentation act: [ :presentation |
			(presentation pane port: #activePresentation) value 
				ifNotNil: [ :activePresentation | 
					self debugger cache at: #activePresentationTitle put: activePresentation title ] ] ].
	tabulator transmit from: #main; toOutsidePort: #selection.
]

{ #category : #'private building' }
GTDebuggerVariablesBrowser >> contextEvaluatorIn: aComposite [

	aComposite pharoScript
		doItReceiver: [ self debugger selectedContext receiver ];
		doItContext: [ self debugger selectedContext ];
		smalltalkClass: [ self debugger selectedContext receiver class ];
		variableBindings: [ self debugger selectedContext gtDebuggerDoItBindings ];
		display: [ 
			String streamContents: [ :stream | 
				stream 
					nextPutAll: (GTSnippets snippetAt: self debugger selectedContext receiver class)] ];
		onChangeOfPort: #text act: [ :textPresentation |
				GTSnippets 
					snippetAt: self debugger selectedContext receiver class 
					put: (textPresentation text asString).
				 textPresentation clearUserEdits ];
		installDefaultSelectionActions
]

{ #category : #accessing }
GTDebuggerVariablesBrowser >> debugger [
	^ debugger
]

{ #category : #accessing }
GTDebuggerVariablesBrowser >> debugger: anObject [
	debugger := anObject
]

{ #category : #actions }
GTDebuggerVariablesBrowser >> restoreSettings [
	| currentPane presentationTitle lastActivePresentation |
	
	presentationTitle := self debugger cache at: #activePresentationTitle ifAbsent: [ ^ self ].
	currentPane := self presentations first paneNamed: #main.
	lastActivePresentation := currentPane presentations  presentations
		detect: [ :each | each title = presentationTitle ] 
		ifNone: [ ^ self ].
	currentPane lastActivePresentation: lastActivePresentation
]

{ #category : #accessing }
GTDebuggerVariablesBrowser >> selectedVariableForContext: aContext [ 
	^ (self variablesFromContext: aContext) 
		detect: [ :each | each key = self debugger selectedVariableName ] 
		ifNone: [ cachedVariables detect: [ :each | each key = 'self' ] ifNone: [ nil ] ]
]

{ #category : #actions }
GTDebuggerVariablesBrowser >> updateVariablesForContext [
	cachedVariables := self 
]

{ #category : #'private building' }
GTDebuggerVariablesBrowser >> variablesFromContext: aContext [ 
	^ cachedVariables ifNil: [ cachedVariables := self buildVariablesFromContext: aContext ]
]

{ #category : #'private building' }
GTDebuggerVariablesBrowser >> variablesIn: composite [
	^ composite fastTable
		useCache;
		display: [ :aContext | self variablesFromContext: aContext. ];
		column: 'Type'
			evaluated: [ '' ]
			width: 75
			tags: [ :aNode  | { aNode variableTag } ]
			sortedBy: [ :x :y |  x variableTag < y  variableTag ];
		column: 'Variable' 
			evaluated: [ :aNode | GTObjectPrinter asNonTruncatedTextFrom: aNode key ]
			width: 175;
		column: 'Value' 
			evaluated: [ :aNode | | rawValue displayValue |
				[ rawValue := aNode rawValue. ] 
					on: Error 
					do: [ displayValue := Text string: aNode errorMessage attribute: TextColor red ].
				displayValue ifNil: [ GTObjectPrinter asTruncatedTextFrom: rawValue ] ];
		dynamicActionsOnSelection: [ :presentation |
			(presentation rawSelection isKindOf: GTInspectorVariableNode) 
				ifTrue: [presentation rawSelection inspectorActionsForNode ]
				ifFalse: [ #() ] ];
		morphicSelectionAct: [:list | list selection value browse ] 
			icon: GLMUIThemeExtraIcons glamorousBrowse 
			on: $b 
			entitled: 'Browse';
		morphicSelectionAct: [:list | list selection value inspect ] 
			icon: GLMUIThemeExtraIcons glamorousInspect 
			on: $i 
			entitled: 'Inspect';
		onChangeOfPort: #rawSelection 
			act: [ :aPresentation | 
				self debugger selectedVariableName: aPresentation rawSelection key ] 
]

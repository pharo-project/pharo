"
I am a debugger that provides the ability to step through the execution at the bytecode level (e.g. one bytecode at a time).
"
Class {
	#name : #GTBytecodeDebugger,
	#superclass : #GTGenericStackDebugger,
	#category : #'GT-BytecodeDebugger-UI'
}

{ #category : #accessing }
GTBytecodeDebugger class >> defaultTitle [

	^ 'Bytecode'
]

{ #category : #accessing }
GTBytecodeDebugger class >> helpClass [
	^ GTBytecodeDebuggerHelp
]

{ #category : #'tools registry' }
GTBytecodeDebugger class >> register [

	self registerToolsOn: Smalltalk tools.
]

{ #category : #'tools registry' }
GTBytecodeDebugger class >> registerToolsOn: registry [

	"Add ourselves to registry. See [Smalltalk tools]" 
	registry registerDebugger: self withRank: 5.
]

{ #category : #accessing }
GTBytecodeDebugger class >> sessionClass [

	^ GTBytecodeDebuggerSession
]

{ #category : #accessing }
GTBytecodeDebugger class >> variablesBrowserClass [

	^ GTBytecodeDebuggerVariablesBrowser
]

{ #category : #'building actions' }
GTBytecodeDebugger >> bytecodeActionsPragmas [

	^ #( bytecodeCodeDebuggingAction )
]

{ #category : #'building presentations' }
GTBytecodeDebugger >> bytecodeIn: aComposite forContext: aContext [

	(self methodBytecodeWidgetIn: aComposite forContext: aContext)
		initialize: [ :bytecodePresentation |
				bytecodePresentation selection: (self session symbolicBytecodeForCurrentPCFrom: self currentContext) ];
		with: [ :bytecode | self installActionsForRootPragmas: self bytecodeActionsPragmas in: bytecode ].
]

{ #category : #accessing }
GTBytecodeDebugger >> bytecodePane [

	^ self browser paneNamed: #bytecode
]

{ #category : #accessing }
GTBytecodeDebugger >> bytecodePresentation [
	
	^ self bytecodePane presentations first
]

{ #category : #building }
GTBytecodeDebugger >> debuggerStructureIn: browser [
	
	browser
		row: #stack;
		row: [ :c | c
			column: #code; 
			column: #bytecode ] span: 2;
		row: #inspector.
		
]

{ #category : #building }
GTBytecodeDebugger >> debuggerTransmissionsIn: browser [

	super debuggerTransmissionsIn: browser.
	browser transmit
		from: #stack port: #selection;
		to: #bytecode;
		andShow: [ :composite :aContext | self bytecodeIn: composite forContext: aContext ]

]

{ #category : #'building presentations' }
GTBytecodeDebugger >> decompiledCodeWidgetIn: arg1 forContext: arg2 [
	| tmp2 |
	tmp2 := arg1 pharoMethod.
	tmp2
		title: 'Decompiled';
		format: [ arg2 compiledCode codeForNoSource ];
		smalltalkClass: [ arg2 methodClass ];
		doItReceiver: [ arg2 receiver ].
	^ tmp2 doItContext: [ arg2 ]
]

{ #category : #'printing/formatting' }
GTBytecodeDebugger >> formatBytecode: aBytecode [
	| textAttributes|
	
	textAttributes := {TextColor new color: self theme textColor}.
	self selectedContext pc = aBytecode offset ifTrue: [ textAttributes := {TextColor new color: self theme textColor. TextEmphasis italic} ]. 
	self selectedContext pc > aBytecode offset ifTrue: [ textAttributes := {TextColor new color: Color gray} ].
	^ Text 
		string: aBytecode printString
		attributes: textAttributes
]

{ #category : #'building presentations' }
GTBytecodeDebugger >> methodBytecodeWidgetIn: arg1 forContext: arg2 [
	| tmp2 |
	tmp2 := arg1 tree.
	tmp2
		title: 'Bytecode';
		rootsExpanded;
		display: [ arg2 compiledCode symbolicBytecodes ];
		children: [ :arg3 | 
			arg3 isArray
				ifTrue: [ arg3 ]
				ifFalse: #() ];
		format: [ :arg4 | self formatBytecode: arg4 ].
	^ tmp2
		onChangeOfPort: #selection
		act: [ :arg5 | 
			| tmp3 |
			self codePresentation
				selectionInterval:
					((tmp3 := arg5 selection)
						ifNil: [ 1 to: 0 ]
						ifNotNil: [ tmp3 sourceInterval ]) ]
]

{ #category : #'building presentations' }
GTBytecodeDebugger >> methodCodeIn: composite forContext: aContext [

	(self methodCodeWidgetIn: composite forContext: aContext)
		with: [ :code | self installCodeActionsFor: code ].
	"self decompiledCodeWidgetIn: composite forContext: aContext"
]

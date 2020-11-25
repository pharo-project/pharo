Class {
	#name : #StSindarinBytecodeDebuggerPresenter,
	#superclass : #SpPresenter,
	#traits : 'TStDebuggerExtension',
	#classTraits : 'TStDebuggerExtension classTrait',
	#instVars : [
		'bytecode',
		'bcContextInspection',
		'toolbar',
		'sindarinDebugger',
		'stDebugger',
		'bytecodeList',
		'currentPC'
	],
	#category : #'NewTools-Sindarin-Tools'
}

{ #category : #'debugger extension' }
StSindarinBytecodeDebuggerPresenter class >> defaultDisplayOrder [

	^ 2
]

{ #category : #specs }
StSindarinBytecodeDebuggerPresenter class >> defaultSpec [
	^ SpBoxLayout newVertical
		add: #toolbar expand: false fill: false padding: 0;
		add: #bytecode;		
		add: #bcContextInspection;
		yourself
]

{ #category : #accessing }
StSindarinBytecodeDebuggerPresenter >> debugger [ 
	^stDebugger
]

{ #category : #'debugger extension' }
StSindarinBytecodeDebuggerPresenter >> debuggerExtensionToolName [
	^ 'Bytecode' 
]

{ #category : #initialization }
StSindarinBytecodeDebuggerPresenter >> initializePresenters [
	bytecode := self newList.
	bcContextInspection := nil inspectionRaw.
	bcContextInspection owner: self.
	self initializeToolbar
]

{ #category : #initialization }
StSindarinBytecodeDebuggerPresenter >> initializeToolbar [
	toolbar := self newToolbar.
	toolbar beIcons.
	SpToolbarPresenterBuilder new
		toolbarPresenter: toolbar;
		visit: self toolbarActions
]

{ #category : #accessing }
StSindarinBytecodeDebuggerPresenter >> setModelBeforeInitialization: aStDebugger [
	"My original model is the debugger presenter that I extend"
	stDebugger := aStDebugger.
	sindarinDebugger := aStDebugger sindarinDebugger
]

{ #category : #accessing }
StSindarinBytecodeDebuggerPresenter >> sindarin [
	^sindarinDebugger 
]

{ #category : #actions }
StSindarinBytecodeDebuggerPresenter >> stepBytecode [
	self sindarin stepBytecode.
	self debugger forceSessionUpdate 
]

{ #category : #initialization }
StSindarinBytecodeDebuggerPresenter >> toolbarActions [
	| group |
	group := (CmCommandGroup named: 'Bytecode Toolbar') asSpecGroup.
	
	group register: (SindarinStepBytecodeCommand forSpecContext: self).

	
	^ group
]

{ #category : #updating }
StSindarinBytecodeDebuggerPresenter >> updateBytecode [

	| selectionIndex |
	bytecodeList := self sindarin currentBytecode.
	currentPC := self sindarin pc.
	bytecode
		items: bytecodeList;
		displayIcon: [ :elem | 
			elem offset = currentPC
				ifTrue: [ Smalltalk iconNamed: #glamorousRight ]
				ifFalse: [ Smalltalk iconNamed: #blank ] ].
	bcContextInspection model:
		(StSindarinBytecodeContextInspectorModel on: self sindarin context).
	selectionIndex := currentPC - bytecodeList first offset + 1.
	selectionIndex <= bytecodeList size ifTrue: [ 
		bytecode selectIndex: selectionIndex ]
]

{ #category : #updating }
StSindarinBytecodeDebuggerPresenter >> updatePresenter [
	super updatePresenter.
	self updateBytecode
]

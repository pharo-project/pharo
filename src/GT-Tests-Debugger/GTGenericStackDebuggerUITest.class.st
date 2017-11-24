Class {
	#name : #GTGenericStackDebuggerUITest,
	#superclass : #GLMMorphicTest,
	#category : #'GT-Tests-Debugger'
}

{ #category : #testing }
GTGenericStackDebuggerUITest >> testNoOverlappingCodeEditorKeyBindings [

	| context process codeEditor keymaps doubleKeymaps |
	
		self skip.
	context := [ 20 factorial ] asContext.
	process := Process
		forContext: context
		priority: Processor userInterruptPriority.
	window := GTGenericStackDebugger openOn: (process newDebugSessionNamed: 'test debugging' startedAt: context).
	codeEditor := self findSatisfying: [ :morph | 
		morph model isKindOf: GLMRubricSmalltalkTextModel ] in: window.
	keymaps := OrderedCollection new.
	codeEditor withAllOwnersDo: [:morph | 
		keymaps addAll: morph kmDispatcher directKeymaps allEntries keymaps ].
	doubleKeymaps := (keymaps groupedBy: [:each | each shortcut ]) select: [ :each | each value size > 1 ].
	self assert: doubleKeymaps isEmpty
]

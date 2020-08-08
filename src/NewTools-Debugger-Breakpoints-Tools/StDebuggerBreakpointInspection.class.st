"
Breakpoint inspection specialized for the debugger.
Instead of showing breakpoints and halts in the inspected object, shows them for the whole system.
Data is taken from system's and tools' caches.
"
Class {
	#name : #StDebuggerBreakpointInspection,
	#superclass : #StObjectBreakpointInspection,
	#category : #'NewTools-Debugger-Breakpoints-Tools-Presenters'
}

{ #category : #specs }
StDebuggerBreakpointInspection class >> debuggingExtensionToolName [
	<debuggerExtensionOrder: 3 showByDefault: false>
	^ 'Breakpoints'
]

{ #category : #private }
StDebuggerBreakpointInspection >> breakPointsForModel [
	^ Breakpoint all
]

{ #category : #private }
StDebuggerBreakpointInspection >> buildHaltData [

	^ StHaltCache defaultCache methodsWithHalts values flattened 
		  collect: [ :node | StHaltInspectionItem forHalt: node ]
]

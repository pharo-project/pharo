"
Breakpoint inspection specialized for the debugger.
Instead of showing breakpoints and halts in the inspected object, shows them for the whole system.
Data is taken from system's and tools' caches.
"
Class {
	#name : #StDebuggerBreakpointInspection,
	#superclass : #StObjectBreakpointInspection,
	#traits : 'TStDebuggerExtension',
	#classTraits : 'TStDebuggerExtension classTrait',
	#category : #'NewTools-Debugger-Breakpoints-Tools-Presenters'
}

{ #category : #'debugger extension' }
StDebuggerBreakpointInspection class >> defaultDisplayOrder [

	^ 3
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

{ #category : #'debugger extension' }
StDebuggerBreakpointInspection >> debuggerExtensionToolName [

	^ 'Breakpoints'
]

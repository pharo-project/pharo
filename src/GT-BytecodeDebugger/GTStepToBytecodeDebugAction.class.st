"
I am a debugging action for stepping to a selected bytecode.
"
Class {
	#name : #GTStepToBytecodeDebugAction,
	#superclass : #DebugAction,
	#category : #'GT-BytecodeDebugger-Actions'
}

{ #category : #registration }
GTStepToBytecodeDebugAction class >> gtBytecodeDebuggerActionFor: aDebugger [
	<gtContextBytecodeCodeDebuggingAction>
	
	^ (self forDebugger: aDebugger)
			icon: GLMUIThemeExtraIcons glamorousPlay
]

{ #category : #'as yet unclassified' }
GTStepToBytecodeDebugAction >> appliesToDebugger: aDebugger [

	^ (aDebugger session isContextPostMortem: aDebugger currentContext) not and: [ 
		aDebugger bytecodePresentation selection offset > aDebugger currentContext pc ]
]

{ #category : #accessing }
GTStepToBytecodeDebugAction >> defaultLabel [

	^ 'Step to bytecode'
]

{ #category : #accessing }
GTStepToBytecodeDebugAction >> defaultOrder [

	^ 10
]

{ #category : #actions }
GTStepToBytecodeDebugAction >> executeAction [

	self session
		runToBytecode: self debugger bytecodePresentation selection 
		inContext: self currentContext
]

{ #category : #accessing }
GTStepToBytecodeDebugAction >> id [

	^ GTStepToBytecodeDebugAction name
]

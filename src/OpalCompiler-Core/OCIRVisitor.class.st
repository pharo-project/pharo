"
I visit an IRMethod instructions. See my subclasses for useful cases.
"
Class {
	#name : 'OCIRVisitor',
	#superclass : 'Object',
	#category : 'OpalCompiler-Core-IR-Manipulation',
	#package : 'OpalCompiler-Core',
	#tag : 'IR-Manipulation'
}

{ #category : 'visiting' }
OCIRVisitor >> visitBlockReturnTop: blockReturnTop [
]

{ #category : 'visiting' }
OCIRVisitor >> visitInstruction: instr [
]

{ #category : 'visiting' }
OCIRVisitor >> visitJump: jump [
]

{ #category : 'visiting' }
OCIRVisitor >> visitJumpIf: jumpIf [
]

{ #category : 'visiting' }
OCIRVisitor >> visitMethod: method [
	self visitNodes: method allSequences
]

{ #category : 'visiting' }
OCIRVisitor >> visitNode: elem [
	^ elem accept: self
]

{ #category : 'visiting' }
OCIRVisitor >> visitNodes: col [
	col do: [ :each | self visitNode: each ]
]

{ #category : 'visiting' }
OCIRVisitor >> visitPop: pop [
]

{ #category : 'visiting' }
OCIRVisitor >> visitPopIntoInstVar: instVar [
]

{ #category : 'visiting' }
OCIRVisitor >> visitPopIntoLiteralVariable: litVar [
]

{ #category : 'visiting' }
OCIRVisitor >> visitPopIntoRemoteTemp: remoteTemp [
]

{ #category : 'visiting' }
OCIRVisitor >> visitPopIntoTemp: tmp [
]

{ #category : 'visiting' }
OCIRVisitor >> visitPushArray: pushArray [
]

{ #category : 'visiting' }
OCIRVisitor >> visitPushDup: pushDup [
]

{ #category : 'visiting' }
OCIRVisitor >> visitPushFullClosure: fullBlock [
]

{ #category : 'visiting' }
OCIRVisitor >> visitPushInstVar: pushInstVar [
]

{ #category : 'visiting' }
OCIRVisitor >> visitPushLiteral: pushLiteral [
]

{ #category : 'visiting' }
OCIRVisitor >> visitPushLiteralVariable: pushLiteralVariable [
]

{ #category : 'visiting' }
OCIRVisitor >> visitPushReceiver: receiver [
]

{ #category : 'visiting' }
OCIRVisitor >> visitPushRemoteTemp: pushRemoteTemp [
]

{ #category : 'visiting' }
OCIRVisitor >> visitPushTemp: pushTemp [
]

{ #category : 'visiting' }
OCIRVisitor >> visitPushThisContext: pushThisContext [
]

{ #category : 'visiting' }
OCIRVisitor >> visitPushThisProcess: pushThisProcess [
]

{ #category : 'visiting' }
OCIRVisitor >> visitReturn: return [
]

{ #category : 'visiting' }
OCIRVisitor >> visitReturnInstVar: instVar [
]

{ #category : 'visiting' }
OCIRVisitor >> visitReturnLiteral: lit [
]

{ #category : 'visiting' }
OCIRVisitor >> visitReturnReceiver: rec [
]

{ #category : 'visiting' }
OCIRVisitor >> visitSend: send [
]

{ #category : 'visiting' }
OCIRVisitor >> visitSequence: sequence [
	sequence do: [ :instr | self visitInstruction: instr]
]

{ #category : 'visiting' }
OCIRVisitor >> visitStoreInstVar: storeInstVar [
]

{ #category : 'visiting' }
OCIRVisitor >> visitStoreLiteralVariable: storeLiteralVariable [
]

{ #category : 'visiting' }
OCIRVisitor >> visitStoreRemoteTemp: storeRemoteTemp [
]

{ #category : 'visiting' }
OCIRVisitor >> visitStoreTemp: storeTemp [
]

{ #category : 'visiting' }
OCIRVisitor >> visitTempVector: tempVector [
]

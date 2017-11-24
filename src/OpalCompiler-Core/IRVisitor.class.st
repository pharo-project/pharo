"
I visit an IRMethod instructions and write them out to a print stream.
"
Class {
	#name : #IRVisitor,
	#superclass : #Object,
	#category : #'OpalCompiler-Core-IR-Manipulation'
}

{ #category : #visiting }
IRVisitor >> visitBlockReturnTop: blockReturnTop [
]

{ #category : #visiting }
IRVisitor >> visitInstruction: instr [
	 
]

{ #category : #visiting }
IRVisitor >> visitJump: jump [
]

{ #category : #visiting }
IRVisitor >> visitJumpIf: jumpIf [
]

{ #category : #visiting }
IRVisitor >> visitMethod: method [
	self visitNodes: method allSequences.
]

{ #category : #visiting }
IRVisitor >> visitNode: elem [
	^ elem accept: self
]

{ #category : #visiting }
IRVisitor >> visitNodes: col [
	col do: [ :each | self visitNode: each ]
]

{ #category : #visiting }
IRVisitor >> visitPop: pop [
]

{ #category : #visiting }
IRVisitor >> visitPopIntoInstVar: instVar [
]

{ #category : #visiting }
IRVisitor >> visitPopIntoLiteralVariable: litVar [
]

{ #category : #visiting }
IRVisitor >> visitPopIntoRemoteTemp: remoteTemp [
]

{ #category : #visiting }
IRVisitor >> visitPopIntoTemp: tmp [
]

{ #category : #visiting }
IRVisitor >> visitPushArray: pushArray [
]

{ #category : #visiting }
IRVisitor >> visitPushClosureCopy: pushClosureCopy [
]

{ #category : #visiting }
IRVisitor >> visitPushDup: pushDup [
]

{ #category : #visiting }
IRVisitor >> visitPushFullClosure: fullBlock [
]

{ #category : #visiting }
IRVisitor >> visitPushInstVar: pushInstVar [
]

{ #category : #visiting }
IRVisitor >> visitPushLiteral: pushLiteral [
]

{ #category : #visiting }
IRVisitor >> visitPushLiteralVariable: pushLiteralVariable [
]

{ #category : #visiting }
IRVisitor >> visitPushReceiver: receiver [
]

{ #category : #visiting }
IRVisitor >> visitPushRemoteTemp: pushRemoteTemp [
]

{ #category : #visiting }
IRVisitor >> visitPushTemp: pushTemp [
]

{ #category : #visiting }
IRVisitor >> visitPushThisContext: pushThisContext [
]

{ #category : #visiting }
IRVisitor >> visitReturn: return [
]

{ #category : #visiting }
IRVisitor >> visitReturnInstVar: instVar [
]

{ #category : #visiting }
IRVisitor >> visitReturnLiteral: lit [
]

{ #category : #visiting }
IRVisitor >> visitReturnReceiver: rec [
]

{ #category : #visiting }
IRVisitor >> visitSend: send [
]

{ #category : #visiting }
IRVisitor >> visitSequence: sequence [
	sequence do: [ :instr | self visitInstruction: instr].
]

{ #category : #visiting }
IRVisitor >> visitStoreInstVar: storeInstVar [
]

{ #category : #visiting }
IRVisitor >> visitStoreLiteralVariable: storeLiteralVariable [
]

{ #category : #visiting }
IRVisitor >> visitStoreRemoteTemp: storeRemoteTemp [
]

{ #category : #visiting }
IRVisitor >> visitStoreTemp: storeTemp [
]

{ #category : #visiting }
IRVisitor >> visitTempVector: tempVector [
]

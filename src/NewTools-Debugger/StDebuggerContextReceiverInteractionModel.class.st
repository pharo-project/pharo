Class {
	#name : #StDebuggerContextReceiverInteractionModel,
	#superclass : #StDebuggerContextInteractionModel,
	#category : #'NewTools-Debugger-Model'
}

{ #category : #accessing }
StDebuggerContextReceiverInteractionModel >> doItContext [

	^ self context receiver
]

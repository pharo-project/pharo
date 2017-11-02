"
I am modelling the push self bytecode
"
Class {
	#name : #IRPushReceiver,
	#superclass : #IRAccess,
	#category : 'OpalCompiler-Core-IR-Nodes'
}

{ #category : #visiting }
IRPushReceiver >> accept: aVisitor [
	^ aVisitor visitPushReceiver: self
]

{ #category : #testing }
IRPushReceiver >> canBeQuickReturn [
	^ true
]

{ #category : #testing }
IRPushReceiver >> isSelf [
	^true
]

{ #category : #accessing }
IRPushReceiver >> quickRetNode [
	^ IRReturnReceiver new
	
]

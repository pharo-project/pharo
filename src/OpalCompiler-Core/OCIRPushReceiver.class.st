"
I model the push self bytecode.
"
Class {
	#name : 'OCIRPushReceiver',
	#superclass : 'OCIRAccess',
	#category : 'OpalCompiler-Core-IR-Nodes',
	#package : 'OpalCompiler-Core',
	#tag : 'IR-Nodes'
}

{ #category : 'visiting' }
OCIRPushReceiver >> accept: aVisitor [
	^ aVisitor visitPushReceiver: self
]

{ #category : 'testing' }
OCIRPushReceiver >> canBeQuickReturn [
	^ true
]

{ #category : 'testing' }
OCIRPushReceiver >> isRemovableByPop [

	^ true
]

{ #category : 'testing' }
OCIRPushReceiver >> isSelf [
	^true
]

{ #category : 'accessing' }
OCIRPushReceiver >> quickRetNode [
	^ OCIRReturnReceiver new
]

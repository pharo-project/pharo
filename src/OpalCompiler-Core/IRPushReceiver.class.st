"
I am modelling the push self bytecode
"
Class {
	#name : 'IRPushReceiver',
	#superclass : 'IRAccess',
	#category : 'OpalCompiler-Core-IR-Nodes',
	#package : 'OpalCompiler-Core',
	#tag : 'IR-Nodes'
}

{ #category : 'visiting' }
IRPushReceiver >> accept: arg1 [

	^ arg1 visitPushReceiver: self
]

{ #category : 'testing' }
IRPushReceiver >> canBeQuickReturn [

	^ true
]

{ #category : 'testing' }
IRPushReceiver >> isRemovableByPop [

	^ true
]

{ #category : 'testing' }
IRPushReceiver >> isSelf [

	^ true
]

{ #category : 'accessing' }
IRPushReceiver >> quickRetNode [

	^ IRReturnReceiver new
]

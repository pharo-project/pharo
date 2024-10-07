"
I model push thisThisProcess bytecode
"
Class {
	#name : 'IRPushThisProcess',
	#superclass : 'IRAccess',
	#category : 'OpalCompiler-Core-IR-Nodes',
	#package : 'OpalCompiler-Core',
	#tag : 'IR-Nodes'
}

{ #category : 'visiting' }
IRPushThisProcess >> accept: arg1 [

	^ arg1 visitPushThisProcess: self
]

{ #category : 'testing' }
IRPushThisProcess >> isRemovableByPop [

	^ true
]

"
push temp
"
Class {
	#name : 'IRPushTemp',
	#superclass : 'IRTempAccess',
	#category : 'OpalCompiler-Core-IR-Nodes',
	#package : 'OpalCompiler-Core',
	#tag : 'IR-Nodes'
}

{ #category : 'visiting' }
IRPushTemp >> accept: arg1 [

	^ arg1 visitPushTemp: self
]

{ #category : 'testing' }
IRPushTemp >> isRemovableByPop [

	^ true
]

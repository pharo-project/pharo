"
I represent the creation and the push on stack of a FullBlockClosure.


"
Class {
	#name : 'IRPushFullClosure',
	#superclass : 'IRInstruction',
	#instVars : [
		'compiledBlock',
		'copiedValues',
		'outerContextNeeded'
	],
	#category : 'OpalCompiler-Core-IR-Nodes',
	#package : 'OpalCompiler-Core',
	#tag : 'IR-Nodes'
}

{ #category : 'visiting' }
IRPushFullClosure >> accept: arg1 [

	^ arg1 visitPushFullClosure: self
]

{ #category : 'accessing' }
IRPushFullClosure >> compiledBlock [

	^ compiledBlock
]

{ #category : 'accessing' }
IRPushFullClosure >> compiledBlock: arg1 [

	compiledBlock := arg1
]

{ #category : 'accessing' }
IRPushFullClosure >> copiedValues [

	^ copiedValues
]

{ #category : 'accessing' }
IRPushFullClosure >> copiedValues: arg1 [

	copiedValues := arg1
]

{ #category : 'debugging' }
IRPushFullClosure >> indexForVarNamed: arg1 [

	^ sourceNode ir indexForVarNamed: arg1
]

{ #category : 'accessing' }
IRPushFullClosure >> outerContextNeeded [

	^ outerContextNeeded == nil or: [ outerContextNeeded ]
]

{ #category : 'accessing' }
IRPushFullClosure >> outerContextNeeded: arg1 [

	outerContextNeeded := arg1
]

{ #category : 'scoping' }
IRPushFullClosure >> remapCopiedValueAt: arg1 oldOne: arg2 newOne: arg3 [

	self copiedValues at: arg1 put: arg3
]

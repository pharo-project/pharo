"
I model the tempVector.

The tempvector is an array that stores all escaping variables of a block that are written to from outside.
"
Class {
	#name : 'IRTempVector',
	#superclass : 'IRInstruction',
	#instVars : [
		'name',
		'vars'
	],
	#category : 'OpalCompiler-Core-IR-Nodes',
	#package : 'OpalCompiler-Core',
	#tag : 'IR-Nodes'
}

{ #category : 'visiting' }
IRTempVector >> accept: arg1 [

	^ arg1 visitTempVector: self
]

{ #category : 'accessing' }
IRTempVector >> indexForVarNamed: arg1 [

	^ vars indexOf: arg1
]

{ #category : 'testing' }
IRTempVector >> isTempVector [

	^ true
]

{ #category : 'accessing' }
IRTempVector >> name [

	^ name
]

{ #category : 'accessing' }
IRTempVector >> name: arg1 [

	name := arg1
]

{ #category : 'accessing' }
IRTempVector >> vars [

	^ vars
]

{ #category : 'accessing' }
IRTempVector >> vars: arg1 [

	vars := arg1
]

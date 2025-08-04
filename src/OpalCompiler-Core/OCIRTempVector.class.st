"
I model the tempVector.

The tempvector is an array that stores all escaping variables of a block that are written to from outside.
"
Class {
	#name : 'OCIRTempVector',
	#superclass : 'OCIRInstruction',
	#instVars : [
		'name',
		'vars'
	],
	#category : 'OpalCompiler-Core-IR-Nodes',
	#package : 'OpalCompiler-Core',
	#tag : 'IR-Nodes'
}

{ #category : 'visiting' }
OCIRTempVector >> accept: aVisitor [
	^ aVisitor visitTempVector: self
]

{ #category : 'accessing' }
OCIRTempVector >> indexForVarNamed: aName [
	^vars indexOf: aName
]

{ #category : 'testing' }
OCIRTempVector >> isTempVector [
	^true
]

{ #category : 'accessing' }
OCIRTempVector >> name [
	^ name
]

{ #category : 'accessing' }
OCIRTempVector >> name: anObject [
	name := anObject
]

{ #category : 'accessing' }
OCIRTempVector >> vars [
	^ vars
]

{ #category : 'accessing' }
OCIRTempVector >> vars: anObject [
	vars := anObject
]

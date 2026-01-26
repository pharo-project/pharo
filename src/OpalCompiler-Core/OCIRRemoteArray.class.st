"
I model the TempVector for the decompiler
"
Class {
	#name : 'OCIRRemoteArray',
	#superclass : 'Object',
	#instVars : [
		'size',
		'index'
	],
	#category : 'OpalCompiler-Core-Bytecode',
	#package : 'OpalCompiler-Core',
	#tag : 'Bytecode'
}

{ #category : 'enumerating' }
OCIRRemoteArray >> do: aBlock [
	1 to: size do: [ :idx | aBlock value: (idx - 1) ]
]

{ #category : 'accessing' }
OCIRRemoteArray >> index [
	^ index
]

{ #category : 'accessing' }
OCIRRemoteArray >> index: anIndex [
	index := anIndex
]

{ #category : 'enumerating' }
OCIRRemoteArray >> indexOf: anInteger [
	^anInteger + 1
]

{ #category : 'accessing' }
OCIRRemoteArray >> size [
	^ size
]

{ #category : 'accessing' }
OCIRRemoteArray >> size: aSize [
	size := aSize
]

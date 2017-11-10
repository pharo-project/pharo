"
I model the TempVector for the decompiler
"
Class {
	#name : #IRRemoteArray,
	#superclass : #Object,
	#instVars : [
		'size',
		'index'
	],
	#category : #OpalCompiler-Core-Bytecode
}

{ #category : #enumerating }
IRRemoteArray >> do: aBlock [
	1 to: size do: [ :idx | aBlock value: (idx - 1) ]
]

{ #category : #accessing }
IRRemoteArray >> index [
	^ index
]

{ #category : #accessing }
IRRemoteArray >> index: anIndex [
	index := anIndex
]

{ #category : #enumerating }
IRRemoteArray >> indexOf: anInteger [
	^anInteger + 1.
]

{ #category : #accessing }
IRRemoteArray >> size [
	^ size
]

{ #category : #accessing }
IRRemoteArray >> size: aSize [
	size := aSize
]

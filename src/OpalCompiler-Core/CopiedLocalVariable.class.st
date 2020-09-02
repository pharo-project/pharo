"
A copying variable is an arg or temp var that is copied into a block that later reads this variable
"
Class {
	#name : #CopiedLocalVariable,
	#superclass : #LocalVariable,
	#instVars : [
		'originalVar'
	],
	#category : #'OpalCompiler-Core-Semantics'
}

{ #category : #queries }
CopiedLocalVariable >> definingNode [
	^originalVar definingNode
]

{ #category : #accessing }
CopiedLocalVariable >> index: anIndex [
	self scope == originalVar scope ifTrue: [ originalVar index: anIndex ].
	super index: anIndex
]

{ #category : #testing }
CopiedLocalVariable >> isArgumentVariable [
	^originalVar isArgumentVariable
]

{ #category : #testing }
CopiedLocalVariable >> isCopying [
	^true
]

{ #category : #'read/write usage' }
CopiedLocalVariable >> isUninitialized [
	^originalVar isUninitialized
]

{ #category : #testing }
CopiedLocalVariable >> isWritable [
	^originalVar isWritable
]

{ #category : #accessing }
CopiedLocalVariable >> originalVar [
	^ originalVar
]

{ #category : #accessing }
CopiedLocalVariable >> originalVar: anObject [
	originalVar := anObject
]

{ #category : #debugging }
CopiedLocalVariable >> writeFromContext: aContext scope: contextScope value: aValue [
	self isWritable ifFalse: [ 	^self error: 'Arguments are read only' ].
	"we need to change this var, all the other copies, and the orginal"
	^contextScope setCopyingTempToAllScopesUpToDefTemp: originalVar to: aValue from: aContext
]

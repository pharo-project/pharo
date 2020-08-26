"
A copying variable is an arg or temp var that is copied into a block that later reads this variable
"
Class {
	#name : #CopyingLocalVariable,
	#superclass : #LocalVariable,
	#instVars : [
		'originalVar'
	],
	#category : #'OpalCompiler-Core-Semantics'
}

{ #category : #queries }
CopyingLocalVariable >> definingNode [
	^originalVar definingNode
]

{ #category : #accessing }
CopyingLocalVariable >> index: anIndex [
	self scope == originalVar scope ifTrue: [ originalVar index: anIndex ].
	super index: anIndex
]

{ #category : #testing }
CopyingLocalVariable >> isArgumentVariable [
	^originalVar isArgumentVariable
]

{ #category : #testing }
CopyingLocalVariable >> isCopying [
	^true
]

{ #category : #'read/write usage' }
CopyingLocalVariable >> isUninitialized [
	^originalVar isUninitialized
]

{ #category : #testing }
CopyingLocalVariable >> isWritable [
	^originalVar isWritable
]

{ #category : #accessing }
CopyingLocalVariable >> originalVar [
	^ originalVar
]

{ #category : #accessing }
CopyingLocalVariable >> originalVar: anObject [
	originalVar := anObject
]

{ #category : #debugging }
CopyingLocalVariable >> writeFromContext: aContext scope: contextScope value: aValue [
	self isWritable ifFalse: [ 	^self error: 'Arguments are read only' ].
	"we need to change this var, all the other copies, and the orginal"
	^contextScope setCopyingTempToAllScopesUpToDefTemp: originalVar to: aValue from: aContext
]

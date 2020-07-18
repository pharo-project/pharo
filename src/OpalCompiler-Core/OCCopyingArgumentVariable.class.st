"
A copying arg is an arg that is copied into a block that later reads this variable
"
Class {
	#name : #OCCopyingArgumentVariable,
	#superclass : #ArgumentVariable,
	#instVars : [
		'originalVar'
	],
	#category : #'OpalCompiler-Core-Semantics'
}

{ #category : #accessing }
OCCopyingArgumentVariable >> index: anIndex [
	self scope == originalVar scope ifTrue: [ originalVar index: anIndex ].
	super index: anIndex
]

{ #category : #testing }
OCCopyingArgumentVariable >> isCopying [
	^true
]

{ #category : #accessing }
OCCopyingArgumentVariable >> originalVar [
	^ originalVar
]

{ #category : #accessing }
OCCopyingArgumentVariable >> originalVar: anObject [
	originalVar := anObject
]

{ #category : #debugging }
OCCopyingArgumentVariable >> writeFromContext: aContext scope: contextScope value: aValue [

	"we need to change this var, all the other copies, and the orginal"
	^contextScope setCopyingTempToAllScopesUpToDefTemp: originalVar to: aValue from: aContext
]

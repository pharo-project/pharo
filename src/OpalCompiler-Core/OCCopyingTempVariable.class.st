"
A copying temp is a temp that is copied into a block that later reads this variable.

Not: the temp vector is passed as a copying temp, too.
"
Class {
	#name : #OCCopyingTempVariable,
	#superclass : #TemporaryVariable,
	#instVars : [
		'originalVar'
	],
	#category : #'OpalCompiler-Core-Semantics'
}

{ #category : #queries }
OCCopyingTempVariable >> definingNode [
	^originalVar definingNode
]

{ #category : #accessing }
OCCopyingTempVariable >> index: anIndex [
	self scope == originalVar scope ifTrue: [ originalVar index: anIndex ].
	super index: anIndex
]

{ #category : #testing }
OCCopyingTempVariable >> isCopying [
	^true
]

{ #category : #accessing }
OCCopyingTempVariable >> originalVar [
	^ originalVar
]

{ #category : #accessing }
OCCopyingTempVariable >> originalVar: anObject [
	originalVar := anObject
]

{ #category : #debugging }
OCCopyingTempVariable >> writeFromContext: aContext scope: contextScope value: aValue [

	"we need to change this var, all the other copies, and the orginal"
	^contextScope setCopyingTempToAllScopesUpToDefTemp: originalVar to: aValue from: aContext
]

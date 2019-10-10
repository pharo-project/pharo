"
A copying temp is a temp that is copied into a block that later reads this variable.

Not: the temp vector is passed as a copying temp, too.
"
Class {
	#name : #OCCopyingTempVariable,
	#superclass : #OCTempVariable,
	#instVars : [
		'originalVar'
	],
	#category : #'OpalCompiler-Core-Semantics'
}

{ #category : #debugging }
OCCopyingTempVariable >> indexInTempVectorFromIR: aName [
	^(self tempVectorDefinitionScopeForTempStoringIt node irInstruction tempVectorNamed: name) 
		indexForVarNamed: aName
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

{ #category : #'temp vector' }
OCCopyingTempVariable >> tempVectorDefinitionScopeForTempStoringIt [
	"If I am stroring a temp Vector, this method returns the definition scope"
	
	| searchScope |
	self isStoringTempVector ifFalse: [^nil].
	searchScope := scope.
	
	[searchScope isInstanceScope or: [searchScope tempVectorName = name]] 
		whileFalse: [searchScope := searchScope outerScope].
	searchScope isInstanceScope 
		ifTrue: [ ^nil] "not found"
		ifFalse: [^ searchScope ]
]

{ #category : #'temp vector' }
OCCopyingTempVariable >> tempVectorForTempStoringIt [
	"If I am stroring a temp Vector, this method returns this vector"
	^self tempVectorDefinitionScopeForTempStoringIt tempVector
]

{ #category : #debugging }
OCCopyingTempVariable >> writeFromContext: aContext scope: contextScope value: aValue [

	"we need to change this var, all the other copies, and the orginal"
	^contextScope setCopyingTempToAllScopesUpToDefTemp: originalVar to: aValue from: aContext
]

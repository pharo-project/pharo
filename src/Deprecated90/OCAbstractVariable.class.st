"
I am an entry in a Scope that gets associated with variable nodes of the same name. 
"
Class {
	#name : #OCAbstractVariable,
	#superclass : #Variable,
	#instVars : [
		'scope',
		'usage'
	],
	#category : #'Deprecated90-OpalCompiler-Core'
}

{ #category : #testing }
OCAbstractVariable class >> isDeprecated [
	"OCAbstractLocalVariable is now direct subclass of Variable represened various kind of local method vars"
	^true
]

{ #category : #converting }
OCAbstractVariable >> asString [

	^ self name
]

{ #category : #generated }
OCAbstractVariable >> isGlobal [

	self
		deprecated: 'Use #isGlobalVariable instead'
		transformWith: '`@rec isGlobal' -> '`@rec isGlobalVariable'.
	^ self isGlobalVariable
]

{ #category : #generated }
OCAbstractVariable >> isInstance [

	self
		deprecated: 'Use #isInstanceVariable instead'
		transformWith: '`@rec isInstance' -> '`@rec isInstanceVariable'.
	^ self isInstanceVariable
]

{ #category : #'read/write usage' }
OCAbstractVariable >> isRead [
	^usage = #read

]

{ #category : #testing }
OCAbstractVariable >> isRemote [
	^false
]

{ #category : #'read/write usage' }
OCAbstractVariable >> isRepeatedWrite [
	^usage = #repeatedWrite

]

{ #category : #generated }
OCAbstractVariable >> isSelf [

	self
		deprecated: 'Use #isSelfVariable instead'
		transformWith: '`@rec isSelf' -> '`@rec isSelfVariable'.
	^ self isSelfVariable
]

{ #category : #generated }
OCAbstractVariable >> isSuper [

	self
		deprecated: 'Use #isSuperVariable instead'
		transformWith: '`@rec isSuper' -> '`@rec isSuperVariable'.
	^ self isSuperVariable
]

{ #category : #generated }
OCAbstractVariable >> isTemp [

	self
		deprecated: 'Use #isTempVariable instead'
		transformWith: '`@rec isTemp' -> '`@rec isTempVariable'.
	^ self isTempVariable
]

{ #category : #generated }
OCAbstractVariable >> isUndeclared [

	self
		deprecated: 'Use #isUndeclaredVariable instead'
		transformWith: '`@rec isUndeclared' -> '`@rec isUndeclaredVariable'.
	^ self isUndeclaredVariable
]

{ #category : #'read/write usage' }
OCAbstractVariable >> isWrite [
	^ usage = #write or: [ self isRepeatedWrite ]
]

{ #category : #'read/write usage' }
OCAbstractVariable >> markRead [
	"reading does not change a #write, nor an #arg"
	usage ifNil: [usage := #read]
]

{ #category : #'read/write usage' }
OCAbstractVariable >> markRepeatedWrite [
	"blocks in a loop have to know if write usage happens outside (just #write) or
	in the loop -> #repeatedWrite"
	usage := #repeatedWrite.
]

{ #category : #'read/write usage' }
OCAbstractVariable >> markWrite [
	"write is the strongest use: a read is turned into a write"
	usage := #write.
]

{ #category : #printing }
OCAbstractVariable >> printOn: stream [

	stream nextPutAll: self name
]

{ #category : #accessing }
OCAbstractVariable >> scope [

	^ scope
]

{ #category : #initializing }
OCAbstractVariable >> scope: aLexicalScope [

	scope := aLexicalScope
]

{ #category : #accessing }
OCAbstractVariable >> usage [

	^ usage
]

{ #category : #accessing }
OCAbstractVariable >> usage: anObject [

	usage := anObject
]

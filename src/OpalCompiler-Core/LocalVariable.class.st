"
I am an argument or temporary variable of a method or block.

index is not the index in the bytecode but just used for sorting
(see tempVars). to be fixed...
"
Class {
	#name : #LocalVariable,
	#superclass : #Variable,
	#instVars : [
		'scope',
		'usage'
	],
	#category : #'OpalCompiler-Core-Semantics'
}

{ #category : #converting }
LocalVariable >> asString [

	^ self name
]

{ #category : #accessing }
LocalVariable >> definingScope [
	^ scope
]

{ #category : #testing }
LocalVariable >> isLocal [

	^true
]

{ #category : #'read/write usage' }
LocalVariable >> isRead [
	^usage = #read

]

{ #category : #testing }
LocalVariable >> isRemote [
	^false
]

{ #category : #'read/write usage' }
LocalVariable >> isRepeatedWrite [
	^usage = #repeatedWrite

]

{ #category : #'read/write usage' }
LocalVariable >> isUninitialized [

	^ self isWrite not
]

{ #category : #'read/write usage' }
LocalVariable >> isUnused [
	"when the var is never read or written, it is not used.
	Note: we have a special #arg use which means arguments are never unused"
	^ usage isNil
]

{ #category : #'read/write usage' }
LocalVariable >> isWrite [
	^ usage = #write or: [ self isRepeatedWrite ]
]

{ #category : #'read/write usage' }
LocalVariable >> markRead [
	"reading does not change a #write, nor an #arg"
	usage ifNil: [usage := #read]
]

{ #category : #'read/write usage' }
LocalVariable >> markRepeatedWrite [
	"blocks in a loop have to know if write usage happens outside (just #write) or
	in the loop -> #repeatedWrite"
	usage := #repeatedWrite.
]

{ #category : #'read/write usage' }
LocalVariable >> markWrite [
	"write is the strongest use: a read is turned into a write"
	usage := #write.
]

{ #category : #printing }
LocalVariable >> printOn: stream [

	stream nextPutAll: self name
]

{ #category : #accessing }
LocalVariable >> scope [

	^ scope
]

{ #category : #initializing }
LocalVariable >> scope: aLexicalScope [

	scope := aLexicalScope
]

{ #category : #accessing }
LocalVariable >> usage [

	^ usage
]

{ #category : #accessing }
LocalVariable >> usage: anObject [

	usage := anObject
]

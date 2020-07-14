"
I am an argument or temporary variable of a method or block.

index is not the index in the bytecode but just used for sorting
(see tempVars). to be fixed...
"
Class {
	#name : #OCAbstractLocalVariable,
	#superclass : #Variable,
	#instVars : [
		'scope',
		'usage'
	],
	#category : #'Deprecated90-OpalCompiler-Core'
}

{ #category : #converting }
OCAbstractLocalVariable >> asString [

	^ self name
]

{ #category : #'read/write usage' }
OCAbstractLocalVariable >> isRead [
	^usage = #read

]

{ #category : #testing }
OCAbstractLocalVariable >> isRemote [
	^false
]

{ #category : #'read/write usage' }
OCAbstractLocalVariable >> isRepeatedWrite [
	^usage = #repeatedWrite

]

{ #category : #'read/write usage' }
OCAbstractLocalVariable >> isUninitialized [

	^ self isWrite not
]

{ #category : #'read/write usage' }
OCAbstractLocalVariable >> isUnused [
	"when the var is never read or written, it is not used.
	Note: we have a special #arg use which means arguments are never unused"
	^ usage isNil
]

{ #category : #'read/write usage' }
OCAbstractLocalVariable >> isWrite [
	^ usage = #write or: [ self isRepeatedWrite ]
]

{ #category : #'read/write usage' }
OCAbstractLocalVariable >> markRead [
	"reading does not change a #write, nor an #arg"
	usage ifNil: [usage := #read]
]

{ #category : #'read/write usage' }
OCAbstractLocalVariable >> markRepeatedWrite [
	"blocks in a loop have to know if write usage happens outside (just #write) or
	in the loop -> #repeatedWrite"
	usage := #repeatedWrite.
]

{ #category : #'read/write usage' }
OCAbstractLocalVariable >> markWrite [
	"write is the strongest use: a read is turned into a write"
	usage := #write.
]

{ #category : #printing }
OCAbstractLocalVariable >> printOn: stream [

	stream nextPutAll: self name
]

{ #category : #accessing }
OCAbstractLocalVariable >> scope [

	^ scope
]

{ #category : #initializing }
OCAbstractLocalVariable >> scope: aLexicalScope [

	scope := aLexicalScope
]

{ #category : #accessing }
OCAbstractLocalVariable >> usage [

	^ usage
]

{ #category : #accessing }
OCAbstractLocalVariable >> usage: anObject [

	usage := anObject
]

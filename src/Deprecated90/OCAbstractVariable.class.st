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

{ #category : #'read/write usage' }
OCAbstractVariable >> isUnused [
	"when the var is never read or written, it is not used.
	Note: we have a special #arg use which means arguments are never unused"
	^ usage isNil
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

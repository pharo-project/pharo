"
I am an entry in a Scope that gets associated with variable nodes of the same name. 
"
Class {
	#name : #OCAbstractVariable,
	#superclass : #Object,
	#instVars : [
		'scope',
		'usage'
	],
	#category : #'OpalCompiler-Core-Semantics'
}

{ #category : #accessing }
OCAbstractVariable class >> semanticNodeClass [
	self subclassResponsibility 
]

{ #category : #converting }
OCAbstractVariable >> asString [

	^ self name
]

{ #category : #accessing }
OCAbstractVariable >> definingScope [
	^ scope
]

{ #category : #emitting }
OCAbstractVariable >> emitStore: methodBuilder [

	self subclassResponsibility
]

{ #category : #emitting }
OCAbstractVariable >> emitValue: methodBuilder [

	self subclassResponsibility
]

{ #category : #testing }
OCAbstractVariable >> isArg [
	^false
]

{ #category : #testing }
OCAbstractVariable >> isClassVariable [
	^ false
]

{ #category : #testing }
OCAbstractVariable >> isGlobal [

	^ false
]

{ #category : #testing }
OCAbstractVariable >> isGlobalVariable [
	^ false
]

{ #category : #testing }
OCAbstractVariable >> isInstance [
	^false

]

{ #category : #testing }
OCAbstractVariable >> isLiteralVariable [
	^false
]

{ #category : #testing }
OCAbstractVariable >> isLocal [

	^false
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

{ #category : #testing }
OCAbstractVariable >> isSelf [
	^false
]

{ #category : #testing }
OCAbstractVariable >> isSelfOrSuper [
	
	^ false
]

{ #category : #testing }
OCAbstractVariable >> isSpecialVariable [
	^false
]

{ #category : #testing }
OCAbstractVariable >> isSuper [
	^false
]

{ #category : #testing }
OCAbstractVariable >> isTemp [

	^ false
]

{ #category : #testing }
OCAbstractVariable >> isUndeclared [

	^ false
]

{ #category : #'read/write usage' }
OCAbstractVariable >> isUninitialized [

	^ false
]

{ #category : #'read/write usage' }
OCAbstractVariable >> isUnused [
	"when the var is never read or written, it is not used.
	Note: we have a special #arg use which means arguments are never unused"
	^ usage isNil
]

{ #category : #testing }
OCAbstractVariable >> isWritable [
	^ true
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

{ #category : #accessing }
OCAbstractVariable >> name [

	^ self subclassResponsibility
]

{ #category : #accessing }
OCAbstractVariable >> originalVar [
	^ self
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
OCAbstractVariable >> semanticNodeClass [
	^self class semanticNodeClass
]

{ #category : #accessing }
OCAbstractVariable >> usage [

	^ usage
]

{ #category : #accessing }
OCAbstractVariable >> usage: anObject [

	usage := anObject
]

{ #category : #accessing }
OCAbstractVariable >> variable [
	"I return the first class variable that I model in the case of globals and ivars"
	^self
]

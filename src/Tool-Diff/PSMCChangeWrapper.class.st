"
Abstract superclass for changes wrapper (for patch tree)
"
Class {
	#name : #PSMCChangeWrapper,
	#superclass : #ListItemWrapper,
	#category : #'Tool-Diff-Wrapper'
}

{ #category : #accessing }
PSMCChangeWrapper >> actualClass [
	"Answer the class represented in the receiver."

	^(self operation ifNil: [^nil]) targetClass
]

{ #category : #accessing }
PSMCChangeWrapper >> icon [

	"Answer a form with an icon to represent the receiver"

	| o |

	o := self operation.
	o ifNil: [ ^ self iconNamed: #changeEmptyIcon ].
	o isAddition
		ifTrue: [ ^ self iconNamed: #changeAddIcon ].
	o isRemoval
		ifTrue: [ ^ self iconNamed: #changeRemoveIcon ].
	^ self iconNamed: #changeUpdateIcon
]

{ #category : #testing }
PSMCChangeWrapper >> isConflict [
	"Answer whether the receiver is a conflict item."

	^false
]

{ #category : #accessing }
PSMCChangeWrapper >> model: anObject [
	"Set the model."

	model := anObject
]

{ #category : #accessing }
PSMCChangeWrapper >> operation [
	"Answer the underlying operation."

	self subclassResponsibility 
]

{ #category : #accessing }
PSMCChangeWrapper >> preferredColor [

	| op |
	op := self operation.
	op ifNil: [ ^ nil ].
	(op isConflict and: [ op isResolved not ] ) ifTrue: [  ^ Color red ].
	op remoteChosen ifFalse: [ ^ Color gray ].
	^ nil "use default color "
]

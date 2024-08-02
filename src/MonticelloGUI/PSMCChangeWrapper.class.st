"
Abstract superclass for changes wrapper (for patch tree)
"
Class {
	#name : 'PSMCChangeWrapper',
	#superclass : 'ListItemWrapper',
	#category : 'MonticelloGUI-Diff',
	#package : 'MonticelloGUI',
	#tag : 'Diff'
}

{ #category : 'accessing' }
PSMCChangeWrapper >> actualClass [
	"Answer the class represented in the receiver."

	^(self operation ifNil: [^nil]) targetClass
]

{ #category : 'choosing' }
PSMCChangeWrapper >> chooseLocal [
	"Choose the local version."

	self operation chooseLocal
]

{ #category : 'accessing' }
PSMCChangeWrapper >> chooseRemote [
	"Choose the remote version."

	self operation chooseRemote
]

{ #category : 'choosing' }
PSMCChangeWrapper >> clearChoice [
	"Choose neither version (be in conflict)."

	self operation clearChoice
]

{ #category : 'accessing' }
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

{ #category : 'testing' }
PSMCChangeWrapper >> isConflict [
	"Answer whether the receiver is a conflict item."

	^false
]

{ #category : 'testing' }
PSMCChangeWrapper >> localChosen [
	"Answer whether the local version is chosen."

	^self operation localChosen
]

{ #category : 'accessing' }
PSMCChangeWrapper >> model: anObject [
	"Set the model."

	model := anObject
]

{ #category : 'accessing' }
PSMCChangeWrapper >> operation [
	"Answer the underlying operation."

	self subclassResponsibility
]

{ #category : 'accessing' }
PSMCChangeWrapper >> preferredColor [

	| op |
	op := self operation.
	op ifNil: [ ^ nil ].
	(op isConflict and: [ op isResolved not ] ) ifTrue: [  ^ Color red ].
	op remoteChosen ifFalse: [ ^ Color gray ].
	^ nil "use default color "
]

{ #category : 'testing' }
PSMCChangeWrapper >> remoteChosen [
	"Answer whether the remote version is chosen."

	^self operation remoteChosen
]

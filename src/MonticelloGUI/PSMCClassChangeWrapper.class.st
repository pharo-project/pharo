"
Wrapper for class changes
"
Class {
	#name : 'PSMCClassChangeWrapper',
	#superclass : 'PSMCChangeWrapper',
	#instVars : [
		'contents'
	],
	#category : 'MonticelloGUI-Diff',
	#package : 'MonticelloGUI',
	#tag : 'Diff'
}

{ #category : 'accessing' }
PSMCClassChangeWrapper >> actualClass [
	"Answer the class represented in the receiver."

	^ super actualClass ifNil: [ Smalltalk globals classNamed: self item ]
]

{ #category : 'accessing' }
PSMCClassChangeWrapper >> contents [
	"Answer the contents of the change."

	^contents ifNil: [contents := self gatherContents]
]

{ #category : 'accessing' }
PSMCClassChangeWrapper >> gatherContents [
	"Answer the contents of the change from the model."

	^((self model select: [:i | i targetClassName = self item and: [
			i definition isClassDefinition not]])
		collect: [:o |  o patchWrapper model: self model]) asSortedCollection: [:a :b |
			a asString <= b asString]
]

{ #category : 'accessing' }
PSMCClassChangeWrapper >> icon [
	"Answer a form with an icon to represent the receiver"

	self isConflict
		ifTrue: [ self localChosen
				ifTrue: [ ^ self iconNamed: #changeBlockIcon ] ].
	^ super icon
]

{ #category : 'testing' }
PSMCClassChangeWrapper >> isConflict [
	"Answer whether the receiver is a conflict item."

	| op |
	op := self operation.
	^ op isNotNil and: [op isConflict ]
]

{ #category : 'accessing' }
PSMCClassChangeWrapper >> operation [
	"Answer the patch operation for the class itself or nil if none."

	^ self model detect: [ :i | i targetClassName = self item and: [ i definition isClassDefinition ] ] ifNone: [ nil ]
]

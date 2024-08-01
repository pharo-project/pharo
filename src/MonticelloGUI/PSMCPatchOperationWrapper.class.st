"
Wrapper for patch operations
"
Class {
	#name : 'PSMCPatchOperationWrapper',
	#superclass : 'PSMCChangeWrapper',
	#category : 'MonticelloGUI-Diff',
	#package : 'MonticelloGUI',
	#tag : 'Diff'
}

{ #category : 'converting' }
PSMCPatchOperationWrapper >> asString [
	"Answer the method name."

	^self item definition summary
]

{ #category : 'accessing' }
PSMCPatchOperationWrapper >> icon [
	"Answer a form with an icon to represent the receiver"

	self item localChosen
		ifTrue: [ ^ self iconNamed: #changeBlockIcon ].
	^ super icon
]

{ #category : 'testing' }
PSMCPatchOperationWrapper >> isConflict [
	"Answer whether the receiver is a conflict item."

	^ self operation isConflict
]

{ #category : 'accessing' }
PSMCPatchOperationWrapper >> operation [
	"Answer the pacth operation for the receiver or nil if none."

	^self item
]

Class {
	#name : #PSMCPatchOperationWrapper,
	#superclass : #PSMCChangeWrapper,
	#category : #Tool-Diff
}

{ #category : #converting }
PSMCPatchOperationWrapper >> asString [
	"Answer the method name."
	
	^self item definition summary
]

{ #category : #accessing }
PSMCPatchOperationWrapper >> chooseLocal [
	"Choose the local version."

	self operation chooseLocal
]

{ #category : #accessing }
PSMCPatchOperationWrapper >> chooseRemote [
	"Choose the remote version."

	self operation chooseRemote
]

{ #category : #testing }
PSMCPatchOperationWrapper >> clearChoice [
	"Choose neither version (be in conflict)."

	self operation clearChoice
]

{ #category : #accessing }
PSMCPatchOperationWrapper >> icon [
	"Answer a form with an icon to represent the receiver"

	self item localChosen
		ifTrue: [ ^ self iconNamed: #changeBlockIcon ].
	^ super icon
]

{ #category : #testing }
PSMCPatchOperationWrapper >> isConflict [
	"Answer whether the receiver is a conflict item."

	^ self operation isConflict
]

{ #category : #testing }
PSMCPatchOperationWrapper >> localChosen [
	"Answer whether the local version is chosen."

	^self operation localChosen
]

{ #category : #accessing }
PSMCPatchOperationWrapper >> operation [
	"Answer the pacth operation for the receiver or nil if none."

	^self item
]

{ #category : #testing }
PSMCPatchOperationWrapper >> remoteChosen [
	"Answer whether the remote version is chosen."

	^self operation remoteChosen
]

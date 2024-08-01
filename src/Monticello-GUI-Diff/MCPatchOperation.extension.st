Extension { #name : 'MCPatchOperation' }

{ #category : '*Monticello-GUI-Diff' }
MCPatchOperation >> diff [
	"Open a diff browser on the changes."
]

{ #category : '*Monticello-GUI-Diff' }
MCPatchOperation >> diffFromSource [
	"Answer fromSource of the operation for a diff tool."

	^self fromSource
]

{ #category : '*Monticello-GUI-Diff' }
MCPatchOperation >> diffToSource [
	"Answer toSource of the operation for a diff tool."

	^self toSource
]

{ #category : '*Monticello-GUI-Diff' }
MCPatchOperation >> patchWrapper [
	"Answer a wrapper for a patch tree for the receiver."

	^PSMCPatchOperationWrapper with: self
]

{ #category : '*Monticello-GUI-Diff' }
MCPatchOperation >> shortSummary [
	"Answer a short summary of the receiver."

	^ self definition summary , (self fromSource = self toSource
		   ifTrue: [ ' (revision changed)' ]
		   ifFalse: [ '' ])
]

{ #category : '*Monticello-GUI-Diff' }
MCPatchOperation >> targetClassName [
	"Answer the full class *name* of the target since the class may no longer exist."

	^self definition fullClassName
]

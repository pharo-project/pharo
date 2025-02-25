Extension { #name : 'MCPatchOperation' }

{ #category : '*MonticelloGUI-Diff' }
MCPatchOperation >> diff [
	"Open a diff browser on the changes."
]

{ #category : '*MonticelloGUI-Diff' }
MCPatchOperation >> diffFromSource [
	"Answer fromSource of the operation for a diff tool."

	^self fromSource
]

{ #category : '*MonticelloGUI-Diff' }
MCPatchOperation >> diffToSource [
	"Answer toSource of the operation for a diff tool."

	^self toSource
]

{ #category : '*MonticelloGUI-Diff' }
MCPatchOperation >> patchWrapper [
	"Answer a wrapper for a patch tree for the receiver."

	^PSMCPatchOperationWrapper with: self
]

{ #category : '*MonticelloGUI-Diff' }
MCPatchOperation >> shortSummary [
	"Answer a short summary of the receiver."

	^ self definition summary , (self fromSource = self toSource
		   ifTrue: [ ' (revision changed)' ]
		   ifFalse: [ '' ])
]

{ #category : '*MonticelloGUI' }
MCPatchOperation >> source [
	"I am call on the changes/merge tools of Monticello if you disable the 'Use Polymorph difference tool' on your setting."

	^ self localChosen
		ifTrue: [ self fromSource ]
		ifFalse: [ self sourceText ]
]

{ #category : '*MonticelloGUI' }
MCPatchOperation >> sourceText [
	^ self isClassPatch 
		ifTrue: [(TextDiffBuilder from: self fromSource to: self toSource) buildDisplayPatch]
		ifFalse: [TextDiffBuilder buildDisplayPatchFrom:  self fromSource to: self toSource inClass: self targetClass]

]

{ #category : '*MonticelloGUI-Diff' }
MCPatchOperation >> targetClassName [
	"Answer the full class *name* of the target since the class may no longer exist."

	^self definition fullClassName
]

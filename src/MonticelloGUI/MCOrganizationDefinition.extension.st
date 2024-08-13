Extension { #name : 'MCOrganizationDefinition' }

{ #category : '*MonticelloGUI-Diff' }
MCOrganizationDefinition >> patchWrapper [
	"Answer a wrapper for a patch tree for the receiver."

	^PSMCPatchOperationWrapper with: self
]

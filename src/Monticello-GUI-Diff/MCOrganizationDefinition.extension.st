Extension { #name : 'MCOrganizationDefinition' }

{ #category : '*Monticello-GUI-Diff' }
MCOrganizationDefinition >> patchWrapper [
	"Answer a wrapper for a patch tree for the receiver."

	^PSMCPatchOperationWrapper with: self
]

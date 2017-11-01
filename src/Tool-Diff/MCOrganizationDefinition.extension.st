Extension { #name : #MCOrganizationDefinition }

{ #category : #'*Tool-Diff' }
MCOrganizationDefinition >> patchWrapper [
	"Answer a wrapper for a patch tree for the receiver."

	^PSMCOrganizationChangeWrapper with: self
]

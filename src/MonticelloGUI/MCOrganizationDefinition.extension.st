Extension { #name : #MCOrganizationDefinition }

{ #category : #'*MonticelloGUI' }
MCOrganizationDefinition >> patchWrapper [
	"Answer a wrapper for a patch tree for the receiver."

	^PSMCOrganizationChangeWrapper with: self
]

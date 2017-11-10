Extension { #name : #MCPatch }

{ #category : #'*MonticelloGUI' }
MCPatch >> browse [

	^ (MCPatchBrowser forPatch: self) show
]

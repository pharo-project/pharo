libgit-finalization
finalizeResourceData: aHandle
	"Call the class specific free frunction if aHandle is still a valid external address."
	aHandle isNull ifTrue: [ ^self ].
	self perform: self freeFunctionSelector with: aHandle.
	aHandle beNull
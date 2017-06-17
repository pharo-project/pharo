operations
defaultSignature
	| signaturePointer |
	signaturePointer := ExternalAddress new.
	self withReturnHandlerDo: [ self signature_default: signaturePointer repo: self ].
	^ LGitSignature fromHandle: signaturePointer
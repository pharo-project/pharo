initialize
destroy
	handle ifNotNil:
			[ :validHandle | 
			OSSDL2Driver current unregisterWindowWithId: validHandle windowID.
			validHandle destroy ].
	renderer := nil.
	handle := nil.
	osWindow := nil
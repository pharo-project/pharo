Extension { #name : #BytecodeEncoder }

{ #category : #'*OpalCompiler-Core' }
BytecodeEncoder >> bindAndJuggle: name [
	"This is used to insert a new temp and reorder temps on editing.
	 It doesn't really work for closure compilation since we have multiple
	 locations for temps.  Simply signal a reparse is necessary."

	ReparseAfterSourceEditing signal
]

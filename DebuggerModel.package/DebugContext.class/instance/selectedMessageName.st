accessing
selectedMessageName
	"Answer the message selector of the current context.
	 If the method is unbound we can still usefully answer its old selector."

	^ context methodSelector